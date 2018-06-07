package Math::MikePath;
{
use Math::CubicFormula;
use Math::QuadraticFormula;
use vars qw($VERSION);
use strict;
#use LWP::Simple;
use Carp qw(cluck croak);
use Math::Function::Root qw(BrentsMethod FalsePosition);
use POSIX qw();
$VERSION = '0.1';

our $enableCarefulFofy=1;
our $enableCarefulfofx=1;

sub mergePaths {
	my @segs;
	push(@segs,$_[0]->{pathSegmentSpecs}->[0]);
	foreach (@_) {
		push (@segs,@{$_->{pathSegmentSpecs}}[1 .. scalar(@{$_->{pathSegmentSpecs}}) - 1]);
		}
	my $ret = new Math::MikePath(join('',@segs),$_[0]->{resolution},$_[0]->{precision});
	#foreach (@_) {undef($_);}
	return $ret;
	}
sub new {

	my $class = shift;
	my $self={};
	bless $self,$class;
#	if ($_[0] =~ /^((file|http|ftp)\:.*)/) {
#		$_=get($1);die "Couldn't get file: $1" unless defined $_;
#		s/^#.*\r?\n//g;s/\r?\n//g;
#		push(@_,$_);
#		}
#	else {
		$self->{pathspec} = shift;
#		}
	#print "  making : ",$self->{pathspec},"\n";
	$self->{resolution} = shift;
	$self->{precision} = @_?shift:$self->{resolution}/1000;
	$self->constructSegments($self->{pathspec});
	# not used anymore? at least at this composit level $self->{thetaprecision} = $self->{precision}/$self->{xmax};
	#$self->{length} = getLength($self);
	#my $toff=0;
	#$self->{pathThetaToCompThetaRanges}=[];
	#for (my $i=0;$i<scalar(@{$self->{pathSegments}});$i++) {
	#	$self->{pathThetaToCompThetaRanges}->[scalar(@{$self->{pathThetaToCompThetaRanges}})] = [$toff,$toff + ($self->{pathSegments}->[$i]->{length}/$self->{length})];
	#	$toff += ($self->{pathSegments}->[$i]->{length}/$self->{length});
	#	#now the indexes of those ranges should match the indexes (indeces) of the components
	#	}
	#$self->{pathThetaToCompThetaRanges}->[scalar(@{$self->{pathThetaToCompThetaRanges}})-1]->[1]=1;
	return $self;
	}
sub newlite {
	my $class = shift;
	my $self={};
	bless $self,$class;
	$self->{isLite}=1;
#	if ($_[0] =~ /^((file|http|ftp)\:.*)/) {
#		$_=get($1);die "Couldn't get file: $1" unless defined $_;
#		s/^#.*\r?\n//g;s/\r?\n//g;
#		push(@_,$_);
#		}
#	else {
		$self->{pathspec} = shift;
#		}
	#print "  making : ",$self->{pathspec},"\n";
	$self->{resolution} = shift;
	$self->{precision} = @_?shift:$self->{resolution}/1000;
	$self->constructSegments($self->{pathspec});
	my $toff=0;
	return $self;
	}
sub constructSegments {
	my $self = shift;
	$self->{pathspec} = shift;
	$self->{pathSegmentSpecs} = [];
	@{$self->{pathSegmentSpecs}} = $self->{pathspec} =~ /[MmZzLlHhVvCcSsQqTtAa][0-9, \-\.e]*/g;
	$self->{pathSegments}=[];
	my $lastM=$self->{pathSegmentSpecs}->[0];
	my $lastSegSpec=$self->{pathSegmentSpecs}->[0];
	for (my $i=1;$i<scalar(@{$self->{pathSegmentSpecs}});$i++) {
		my $thisSegSpec=$self->{pathSegmentSpecs}->[$i];
		if ($self->{pathSegmentSpecs}->[$i] =~ /^M/i) {$lastM=$self->{pathSegmentSpecs}->[$i];} #so we can start to be smart about paths with subpaths (multiple Ms)
		if ($self->{pathSegmentSpecs}->[$i] =~ /^(Z)/i) {$thisSegSpec=$1.substr($lastM,1);} # so we can treat it as a LineSegment, and start to be smart about paths with subpaths (multiple Ms). Ee should flag it as special case somehow, but we don't yet
		if ($lastSegSpec =~ /^Z/i && $thisSegSpec !~ /^M/i) {$lastSegSpec=$lastM;} #per SVG spec - if no new M following Z, but something else, that something else is from last M

# set these up so they get handled like linetos
#warn " at $i $thisSegSpec\n";
if ($thisSegSpec=~/^H/i) {
    $thisSegSpec=~s/\s//g;
    if ($i==1) {
        $lastM=~/M\s*?[0-9\-\.eE]+\s*?[\s,]\s*?([0-9\-\.eE]+)/;
        $thisSegSpec.=','.$1;
        }
    else {$thisSegSpec.=','.$self->{pathSegments}->[$i - 2]->{p2}->[1];}
    $thisSegSpec=~s/^H/L/;
#print "h: $thisSegSpec\n";
    }
if ($thisSegSpec=~/^V/i) {
    if ($i==1) {
        $lastM=~/M\s*?([0-9\-\.eE]+)\s*?[\s,]\s*?[0-9\-\.eE]+/;
        my $h = $1;
        $thisSegSpec=~s/^V(.*)$/'L'.$h.','.$1/ei;
        }
    else {
        $thisSegSpec=~s/^V(.*)$/'L'.$self->{pathSegments}->[$i - 2]->{p2}->[0].','.$1/ei;
        }
#print "v: $thisSegSpec\n";
    }


		$self->{pathSegments}->[$i - 1] = $self->constructSegment($thisSegSpec,$lastSegSpec);
		$lastSegSpec=$thisSegSpec;
		}
	$self->{minx} = (        sort {$a<=>$b} map {$_->{minx}} @{$self->{pathSegments}})[0];
	$self->{maxx} = (reverse sort {$a<=>$b} map {$_->{maxx}} @{$self->{pathSegments}})[0];
	$self->{miny} = (        sort {$a<=>$b} map {$_->{miny}} @{$self->{pathSegments}})[0];
	$self->{maxy} = (reverse sort {$a<=>$b} map {$_->{maxy}} @{$self->{pathSegments}})[0];

	$self->{pathspecparts}=[($self->{pathspec}=~/([MmZzLlHhVvCcSsQqTtAa]|[0-9\-\.e]+[, ]+[0-9\-\.e]+)[, ]*/g)];#might be useful for an interpolation function, to avoid some regex's
	#print "pathspec parts: ",join("  ,  ",@{$self->{pathspecparts}}) , "\n";
	$self->{pathspecparts} = [(map {if ($_=~/([0-9\-\.e]+)[, ]+([0-9\-\.e]+)/) {[0,[$1,$2]]} else {[1,$_]}} @{$self->{pathspecparts}})];
	#print "pathspec parts: ",join("  ,  ",map {ref($_->[1])?"$_->[1]->[0],$_->[1]->[1]":$_->[1]} @{$self->{pathspecparts}}) , "\n";

	#print "minx:$self->{minx},maxx:$self->{maxx},miny:$self->{miny},maxy$self->{maxy}\n";
	}
sub parameterize {
	my $self=shift;
	my $newlength=0;
	my $toff=0;
	foreach (@{$self->{pathSegments}}) {$_->{length}=$_->getLength();$newlength+=$_->{length};}
	$self->{length} = $newlength;
    if ($newlength == 0) {croak "zero length for path: ",$self->{pathspec};}
	$self->{pathThetaToCompThetaRanges}=[];
	for (my $i=0;$i<scalar(@{$self->{pathSegments}});$i++) {
		$self->{pathThetaToCompThetaRanges}->[scalar(@{$self->{pathThetaToCompThetaRanges}})] = [$toff,$toff + ($self->{pathSegments}->[$i]->{length}/$self->{length})];
		$toff += ($self->{pathSegments}->[$i]->{length}/$self->{length});
		#now the indeces of those ranges should match the indeces of the components
		}
	$self->{pathThetaToCompThetaRanges}->[scalar(@{$self->{pathThetaToCompThetaRanges}})-1]->[1]=1;
	}
sub getSegsInRange {
	my $self = shift;
	my $point= shift;
	return grep {my ($bx,$by) = $_->inRange($point);$bx || $by} @{$self->{pathSegments}};
	}
sub precision {
	my $self=shift;
	my $np=@_?shift:undef;
	if (defined($np)) {
		$self->{precision}=$np;
		#$self->{thetaprecision} = $self->{precision}/$self->{xmax};
		foreach (@{$self->{pathSegments}}) {
			$_->{precision}=$self->{precision};
			#$_->{thetaprecision}=$self->{thetaprecision};
			}
		}
	return $self->{precision};
	}

sub getLength {

	# NOTE - you now support paths with "M" moveto commands
	# per SVG spec, MoveTo does not contribute to path length
	# new MoveTo package ISA LineSegment, but it's getLength
	# is overridden to return zero.

	my $self=shift;
	my $res = shift;
	if (!defined($res)) {$res=1000;}
	my $length=0;
	foreach (@{$self->{pathSegments}}) {
		$length += $_->getLength($res);
		}
	return $length;
	}

sub dimensionalCurve {
	my $self     = shift;
	my $step     = shift; # constant (scalar) or reference to function of distance along curve - f(t), t:[0.0 to 1.0]
	my $startstep= @_?shift:0; # fraction of first step to offset first point from start
	my $endstep  = @_?shift:undef; # fraction of last step to offset last point from end
	                               # for now, only used if $step is a constant
	my $segrange = @_?shift:[0,scalar(@{$self->{pathSegments}}) - 1]; #2 element array of start and stop indeces of path segments to work over

	# developed in and copied from javascript
    # so keep that synced with any changes here

	my @ret=(); # list of points like [x,y,normal,theta] (theta is (normalized, i.e. fraction of distance) theta on whole path, not from segs)

	my $segslength=0;
	for (my $i=$segrange->[0];$i<=$segrange->[1];$i++) {
        # default theta divisor (sample count) is 1000,
        # but that's not enough it looks like at board scale -
        # determined while working on rail piece design
	    $segslength+=$self->{pathSegments}->[$i]->getLength(10000);
	    }
	my $lengthsum=0;
    if ($segslength < 1/10000) {return ();} # quick way to avoid divide by zero
	my $lengthfraction=$lengthsum/$segslength;
	my $stepfunc=(ref($step))?$step:sub {return $step};

	my $stepval=&{$stepfunc}($lengthfraction);

    # default is first step will be zero-length, giving start point of start segment
    # TODO otherwise, seems like if stepfunc is not constant, any $startstep fraction
    # needs to take into account non-linear step growth... maybe a root find thing and or weighted value thing
    # or maybe the factor multiplies the variable of the step function, and not the span of two evals of that function
    # so see theres stuff to figure here.
	my $firststep = defined($startstep) ? $startstep*$stepval : 0;
	my $laststep;# undef is significant, because "0" needs to be a true last step request - actually we don't make that test anywhere below though

    # Check if the whole path is too short to subdivide by the requested dimension.
	if ($segslength<$firststep) {warn "path too short ($segslength) to divide by first step $firststep\n";return ();}

    # If the step size is fixed, and we want to hit a specified end point,
    # adjust step size to be smaller than requested,
    # to hit that endpoint in a whole number of steps.
	if (!ref($step) && defined $endstep) {
		$laststep = (1-$endstep)*$stepval;
		my $fitlength=$segslength - ($firststep + $laststep);
		my $d = $fitlength/$step;
		if ($d<1) { # Path too short to support both the requested first step offset and last step offset
		    $stepfunc = sub {return $step}; # so this will overshoot on the first step, and result will just have the first step point
		    }
		else {
            my $floored_d = POSIX::floor($d);
			my $r = $d - $floored_d;
            my $newstep = $step;
            if ($r != 0) {
                my $adj = ($step*(1.0 - $r))/($floored_d + 1.0); # because we're shrinking step, one more will fit in
                $newstep = $step - $adj;
                }
			$stepfunc = sub {return $newstep};



            #// a*adjstd + b*adjstd + c*adjstd = segslength, c is number of steps we'll end up with
            #// (a + b + c) * adjstd = segslength
            #// (AB + c) * adjstd = segslength
            #// a and b are known
            #// c can be one more than what you get when you figure this with unadjusted step size
            #// call that C
            #// so,
            #// adjstd = segslength/(A+B+C)
            #// yep. looks reasonable.

            my $C = (POSIX::floor($d) + 1);
            my $ABC = ( $startstep + $endstep + $C );
            my $adjustedstep = $segslength / $ABC;
            $stepfunc = sub {return $adjustedstep};
            #// console.log('adjusted: ' + step + ' to ' + adjustedstep);
            $stepval = &{$stepfunc}($lengthfraction);
            $firststep = $startstep * $adjustedstep;
            $laststep = (1 - $endstep) * $adjustedstep; #// laststep isn't used, is it?


			}
		}
    # Otherwise, if the step size is fixed, and no end point offset is specified
    # use exactly requested step size, and don't care where the last point lands,
    # as long as it's less than stepsize from end of last segment.

    # TODO: (this todo is duplicated in both perl and javascript versions)
    # variable (assumed) step size, so need to smart-figure what first step is, and for last step
    # that depends both on first step and step function, so weirder to figure.
    # maybe analogous to fixed step case, you can find a tiny number to subtract from each input to the eval of the step function
    # to shrink the resulting steps (unless the function's weird and that makes them grow? detect that?)
    # If you pull that off, your rail designer will work like you want, more, sorta.

    # The key feature of that would be that you could do a 50% step on that first step
    # and then a mirror version of the path, mirrored through start point, would
    # have a "natural" looking space between it's first point and the orig first point.
    # "Natural" in view of the way the step function is varying steps in that region.

	#elsif (ref($step) && (defined $endstep || defined $endstep)) {}

	my $zeropt= [@{$self->{pathSegments}->[$segrange->[0]]->point(0)},
	            $self->{pathSegments}->[$segrange->[0]]->angleNormal_byTheta(0),
	            $lengthfraction
	            ];

	if ($stepval == $firststep || (!defined($startstep) || $startstep eq '0')) {push(@ret,$zeropt);}

	my $endpt; # set to each seg endpoint in for loop,
	           # and we might use the last one, after the for loop

	for (my $i=$segrange->[0];$i<=$segrange->[1];$i++) {
		my $prevtheta=0; # seg theta, not path theta
		my $firstpt=[@{$self->{pathSegments}->[$i]->point($prevtheta)},
		            $self->{pathSegments}->[$i]->angleNormal_byTheta($prevtheta),
		            $lengthfraction
		            ];

		my @thispointandtheta=($firstpt,0);
		my @prevptnth=@thispointandtheta;
		$endpt=[@{$self->{pathSegments}->[$i]->point(1)},
		       $self->{pathSegments}->[$i]->angleNormal_byTheta(1),
		       1
		       ];

        # downgrade any BigFloats
#        $_ = sprintf("%.15f",$firstpt->[0]->bstr())
        $_ = sprintf("%.15f",$_->bstr())
            for grep ref($_),
                (@$firstpt,@$endpt);

        my $dist_to_end = sqrt(
                       ($endpt->[1] - $thispointandtheta[0]->[1])**2
                     + ($endpt->[0] - $thispointandtheta[0]->[0])**2
        );

        my $first_while_loop = 1;

		while ( $dist_to_end > $stepval ) {

			my $thisstep=$stepval;
			if ($firststep) {$thisstep=$firststep;$firststep=0;}

            # downgrade any BigFloats
            $_ = sprintf("%.15f",$firstpt->[0]->bstr())
                for grep ref($_),
                    (@{$thispointandtheta[0]},$thispointandtheta[1]);

			@prevptnth=@thispointandtheta;

            warn("ref in dimensionalCurve") for grep ref($_), ($thisstep,$prevtheta);

			@thispointandtheta=$self->{pathSegments}->[$i]->dimensionalStepFromTheta($thisstep,$prevtheta,1);

            # downgrade any BigFloats
#            $_ = sprintf("%.15f",$firstpt->[0]->bstr())
            $_ = sprintf("%.15f",$_->bstr())
                for map {warn "saw Big\n";$_}
                    grep ref($_),
                    (@{$thispointandtheta[0]},$thispointandtheta[1]);

			$lengthsum+=sqrt(($thispointandtheta[0]->[0]-$prevptnth[0]->[0])**2 + ($thispointandtheta[0]->[1]-$prevptnth[0]->[1])**2);
			$lengthfraction=$lengthsum/$segslength;
			$stepval=&{$stepfunc}($lengthfraction);

			push(@{$thispointandtheta[0]},$self->{pathSegments}->[$i]->angleNormal_byTheta($thispointandtheta[1]),$lengthfraction);

            # downgrade any BigFloats
            if (ref($thispointandtheta[0]->[2])) {
                $thispointandtheta[0]->[2]=sprintf("%.15f",$thispointandtheta[0]->[2]->bstr());
                }

			push(@ret,$thispointandtheta[0]);

            # avoid infinite loop when theta isn't changing
            if ($prevtheta == $thispointandtheta[1]
                # A same-theta result of 0 can be valid on the first run through
                # this while loop, if our previous point left us very very close
                # to where this step (which would be a $firststep) being right
                # on this new seg's start point. So that's what this test is for:
                && !($prevtheta == 0 && $first_while_loop)
               ) {
                warn "hit LAST to avoid infinite loop when theta isn't changing\nprob should be die?\n";
                last;
                }

			$prevtheta = $thispointandtheta[1];

            $dist_to_end = sqrt(
                           ($endpt->[1] - $thispointandtheta[0]->[1])**2
                         + ($endpt->[0] - $thispointandtheta[0]->[0])**2
            );

            $first_while_loop = 0;
			}

        # TODO: actually hitting every segment endpoint might be a usful
        #       option to stick right here. You _don't_ want that for what
        #       you're using this for now, mostly, like getting nicely spaced
        #       locations for radial rail pieces. But it would be good for
        #       other things where you need to capture inflection points where
        #       segments meet along a path.

		# While loop ended because next step would have stepped beyond
		# end of current segment. Now set first step for next segment,
		# accounting for whatever partial step distance was taken up by
		# the end of this segment.
		$firststep=$stepval - $dist_to_end;

		}

    # The while loop should _not_ get the last step in this case.
    # (though rarely it might? giving duplicate to this point?)
    if (defined($endstep) && $endstep == 0) {
        push(@ret,$endpt);
        }

	return @ret;
	}

sub f {
	my $self=shift;
	my $x=shift;
	my @res;
	foreach ($self->getSegsInRange([$x,undef])) {
		if (!wantarray) {my $this=$_->f($x);if ($this || $this eq '0') {push(@res,$this);last;} } # call segment function in scalar context, and avoid collecting points after the first one is found
		else {my @these = $_->f($x);push(@res,@these);}
		}
	my %dupsieve;
	@res = grep {!$dupsieve{$_}++} @res;
	return wantarray ? @res : $res[0];
	}
sub F {
	my $self=shift;
	my $y=shift;
	#my $scalefactor = @_?shift:1; #line seems misguided, obsolete
	my @res;
	foreach ($self->getSegsInRange([undef,$y])) {
		my @these;
		if (!wantarray) {$these[0]=$_->F($y);if ($these[0] || $these[0] eq 0) {push(@res,@these);last;} } # call segment function in scalar context, and avoid collecting points after the first one is found
		else {@these = $_->F($y);push(@res,@these);}
		push(@res,@these);
		}
	my %dupsieve;
	@res = grep {!$dupsieve{$_}++} @res;
	return wantarray ? @res : $res[0];
	}
sub point {
	my $self = shift;
	my $theta = shift;
	my @seg;
	if (!defined($self->{pathThetaToCompThetaRanges})) {$self->parameterize();} #delay this until you need it
	@seg=$self->getSegThetaIndexAtPathTheta($theta);
	#print "path->point($theta) find seg?: $seg[0], $seg[1], $seg[2]\n";
	return $seg[0]->point($seg[1]);
	}
sub getSegThetaIndexAtPathTheta {
# how do you map segment's non-uniform theta spacing to your path-wide pseudo-theta indexing?
# here I'm tring to find the segments native theta that correspondes to a given fraction of it's length
# Expensive, but should be doable. Make it work, then decide if it's too expensive.
	my $self = shift;
	my $theta = shift;
	if (!defined($self->{pathThetaToCompThetaRanges})) {$self->parameterize();} #delay this until you need it
	for (my $i=0;$i < scalar(@{$self->{pathThetaToCompThetaRanges}});$i++) {
		#warn "test:  (($self->{pathThetaToCompThetaRanges}->[$i]->[0] < $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[0] eq $theta)  &&  ($self->{pathThetaToCompThetaRanges}->[$i]->[1] > $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[1] eq $theta)) ? ",((($self->{pathThetaToCompThetaRanges}->[$i]->[0] < $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[0] eq $theta) && ($self->{pathThetaToCompThetaRanges}->[$i]->[1] > $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[1] eq $theta))?'yes':'no'),"\n";
		if (
			($self->{pathThetaToCompThetaRanges}->[$i]->[0] < $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[0] eq $theta)
			&&
			($self->{pathThetaToCompThetaRanges}->[$i]->[1] > $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[1] eq $theta)
			) {
			my $segoffsetlengthfraction = ($theta - $self->{pathThetaToCompThetaRanges}->[$i]->[0]) / ($self->{pathThetaToCompThetaRanges}->[$i]->[1] - $self->{pathThetaToCompThetaRanges}->[$i]->[0]);
			my $segtheta=$segoffsetlengthfraction; #might that be good enough? w/o worrying about real length issues?
			return ($self->{pathSegments}->[$i],$segtheta,$i);
			}
		}
	}
sub getPathThetaAtSegTheta {
	my $self = shift;
	my $segindex  = shift;
	my $segtheta = shift;
	if (!defined($self->{pathThetaToCompThetaRanges})) {$self->parameterize();} #delay this until you need it
	my $theta = ( $segtheta * ($self->{pathThetaToCompThetaRanges}->[$segindex]->[1] - $self->{pathThetaToCompThetaRanges}->[$segindex]->[0]) ) + $self->{pathThetaToCompThetaRanges}->[$segindex]->[0];
	#print "getPathThetaAtSegTheta:\n$theta = ( $segtheta * (",$self->{pathThetaToCompThetaRanges}->[$segindex]->[1]," - ",$self->{pathThetaToCompThetaRanges}->[$segindex]->[0],") ) + ",$self->{pathThetaToCompThetaRanges}->[$segindex]->[0],";\n";
	return $theta;
	}
sub curve {
	my $self = shift;
	my $cnt  = @_?shift:20;
	my @ret;
	#my $l=$self->getLength();
	#my $inc=$l/$cnt;
	my $inc=1/$cnt;
	#the sprintf is attmpting to overcome increment overshoot at "1" due to inherent rounding errors
	for (my $i=0;$i<1;$i+=$inc) {push(@ret,$self->point($i));}
	push(@ret,$self->point(1));
	#my $inc = abs($self->{maxx} - $self->{minx})/$cnt;
	#if ($inc>0) {
	#	foreach (my $i=$self->{minx};$i<=$self->{maxx};$i+=$inc) {
	#		#print "f($i) = ",$self->f($i),"\n";
	#		push(@ret,[$i,$self->f($i)]);
	#		if ($i + $inc > $self->{maxx}) {push(@ret,[$self->{maxx},$self->f($self->{maxx})]);last;}
	#		}
	#	}
	#elsif ($inc eq 0) {
	#	$inc = abs($self->{maxy} - $self->{miny})/$cnt;
	#	#print "in MikePath curve(), min and max x were same, so using min ($self->{miny}) and max ($self->{maxy}) y, and F()";
	#	foreach (my $i=$self->{miny};$i<=$self->{maxy};$i+=$inc) {
	#		#print "F($i) = ",$self->F($i),"\n";
	#		push(@ret,[$self->F($i),$i]);
	#		if ($i + $inc > $self->{maxy}) {push(@ret,[$self->F($self->{maxy}),$self->{maxy}]);last;}
	#		}
	#	}
	#else {
	#	foreach (my $i=$self->{maxx};$i<=$self->{minx};$i+=$inc) {
	#		#print "f($i) = ",$self->f($i),"\n";
	#		push(@ret,[$i,$self->f($i)]);
	#		if ($i + $inc < $self->{minx}) {push(@ret,[$self->{minx},$self->f($self->{minx})]);}
	#		}
	#	}
	return @ret;
	}
sub secondDerivative {
	my $self=shift;
	my $x=shift;
	my $y=@_?shift:undef;
	my @res;
	foreach ($self->getSegsInRange([$x,$y])) {push(@res,$_->secondDerivative($x,$y));}
	return wantarray ? @res : $res[0];
	}
sub slopeTangent {
	my $self=shift;
	my $x=shift;
	my $y=@_?shift:undef;
	my @res;
	foreach ($self->getSegsInRange([$x,$y])) {push(@res,$_->slopeTangent($x,$y));}
	return wantarray ? @res : $res[0];
	}
sub slopeNormal  {
	my $self=shift;
	my $x=shift;
	my $y=@_?shift:undef;
	my @res;
	foreach ($self->getSegsInRange([$x,$y])) {push(@res,$_->slopeNormal($x,$y));}
	return wantarray ? @res : $res[0];
	}
sub slopeTangent_byTheta {
	my $self=shift;
	my $theta=shift;
	my @seg=$self->getSegThetaIndexAtPathTheta($theta);
	return $seg[0]->slopeTangent_byTheta($seg[1]);
	}
sub slopeNormal_byTheta  {
	my $self=shift;
	my $theta=shift;
	my @seg=$self->getSegThetaIndexAtPathTheta($theta);
	return $seg[0]->slopeNormal_byTheta($seg[1]);
	}
sub angleTangent {
	my $self=shift;
	my $x=shift;
	my $y=@_?shift:undef;
	my @res;
	foreach ($self->getSegsInRange([$x,$y])) {push(@res,$_->angleTangent($x,$y));}
	return wantarray ? @res : $res[0];
	}
sub angleNormal  {
	my $self=shift;
	my $x=shift;
	my $y=@_?shift:undef;
	my @res;
	foreach ($self->getSegsInRange([$x,$y])) {push(@res,$_->angleNormal($x,$y));}
	return wantarray ? @res : $res[0];
	}
sub angleTangent_byTheta {
	my $self=shift;
	my $theta=shift;
	my @seg=$self->getSegThetaIndexAtPathTheta($theta);
	return $seg[0]->angleTangent_byTheta($seg[1]);
	}
sub angleNormal_byTheta  {
	my $self=shift;
	my $theta=shift;
	my @seg=$self->getSegThetaIndexAtPathTheta($theta);
	return $seg[0]->angleNormal_byTheta($seg[1]);
	}

sub solveXforTheta {
	my $self=shift;
	my $x=shift;
	my @thetas;
	my $toff=0;
	for (my $i=0;$i<scalar(@{$self->{pathSegments}});$i++) {
		my @segthetas;
		my ($bx,$by) = $self->{pathSegments}->[$i]->inRange([$x,undef]);
		if ($bx || $by) {
			push(@segthetas,$self->{pathSegments}->[$i]->solveXforTheta($x));
			}
		foreach (@segthetas) {push(@thetas,$toff + (  $self->{pathSegments}->[$i]->getLength(1000,0,$_) / $self->{length}  ));} #uh, I hope
		$toff+=$self->{pathSegments}->[$i]->{length}/$self->{length};
		#print "toff: $toff after i: $i last length: $self->{pathSegments}->[$i]->{length} self length: $self->{length}\n";
		}
	my %dupsieve;
	@thetas = grep {!$dupsieve{$_}++} @thetas;
	return wantarray ? @thetas:$thetas[0];
	}

sub solveYforTheta {
	my $self=shift;
	my $y=shift;
	my @thetas;
	my $toff=0;
	for (my $i=0;$i<scalar(@{$self->{pathSegments}});$i++) {
		my @segthetas;
		my ($bx,$by) = $self->{pathSegments}->[$i]->inRange([undef,$y]);
		if ($bx || $by) {
			push(@segthetas,$self->{pathSegments}->[$i]->solveYforTheta($y));
			}
		foreach (@segthetas) {push(@thetas,$toff + (  $self->{pathSegments}->[$i]->getLength(100,0,$_) / $self->{length}  ));} #uh, I hope
		$toff+=$self->{pathSegments}->[$i]->{length}/$self->{length};
		}
	my %dupsieve;
	@thetas = grep {!$dupsieve{$_}++} @thetas;
	return wantarray ? @thetas:$thetas[0];
	}

sub normalizeY {
	my $self=shift;
	my $nymin = shift;
	my $nymax = shift;
	my $constrain = @_?shift:0;
	my $scalefactor=1;
	my $ydiff=$self->{maxy} - $self->{miny};
	my $oldx=$self->{minx};
	my $untranslatex=$self->{pathSegments}->[0]->{p1}->[0];
	my $untranslatey=$self->{pathSegments}->[0]->{p1}->[1];
	#$self->translate(-$self->{minx},-$self->{miny});
	$self->translate(-$self->{pathSegments}->[0]->{p1}->[0],-$self->{pathSegments}->[0]->{p1}->[1]);
	if (abs($ydiff) > 0.0000000000001) {$scalefactor = ($nymax - $nymin)/$ydiff;}
	my $newspec='';
	#print "yoldspec:",$self->{pathspec},"\n";
	foreach (@{$self->{pathSegmentSpecs}}) {
		my $segTypeLetter = substr($_,0,1);
		my @thesepoints= $self->extractPointsFromPathSpec($_);
		$newspec.=$segTypeLetter;
		my $first=1;
		foreach my $point (@thesepoints) {
			if (ref($point) eq 'ARRAY') {
				#my $newy=$point->[1] * $scalefactor * ($first && $segTypeLetter eq 'A'?(($self->{maxy} - $self->{miny})/(2*$point->[1])):1);
				my $newy=$point->[1] * $scalefactor;
				my $newx=$point->[0];
				if ($constrain && !($first && $segTypeLetter eq 'A')) {
					$newx*=$scalefactor;
					}
# SPEED ATTEMPTS
#				if (ref($newy) && !ref($point->[1])) {$newy=eval(substr($newy->bstr,0,25));}
#				if (ref($newx) && !ref($point->[0])) {$newx=eval(substr($newx->bstr,0,25));}
				if (ref($newy) && !ref($point->[1])) {$newy=0 + sprintf("%.20f",$newy->bstr);}
				if (ref($newx) && !ref($point->[0])) {$newx=0 + sprintf("%.20f",$newx->bstr);}
				$newspec.=$newx.','.$newy.',';
				}
			else {$newspec.=$point.',';}
			$first=0;
			}
		$newspec=substr($newspec,0,-1);

		##if (!$constrain) {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')? $_->[0].','.eval(substr(''.($_->[1] * $scalefactor),0,25)) : $_} @thesepoints);}
		#if (!$constrain) {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')? $_->[0].','.($_->[1] * $scalefactor) : $_} @thesepoints);}
		##else             {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')?eval(substr(($_->[0] * $scalefactor),0,25)).','.eval(substr(($_->[1] * $scalefactor),0,25)) : $_} @thesepoints);}
		#else             {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')?($_->[0] * $scalefactor).','.($_->[1] * $scalefactor) : $_} @thesepoints);}
		}
	#print "ynewspec:$newspec\n";
	$self->constructSegments($newspec);
	my $offset = $nymin - $self->{miny};
	#$self->translate($oldx,$offset);
	$self->translate($untranslatex,$offset);
	if ($nymin ne $self->{miny}) {
	#	print "UH OH y: tried to set miny to $nymin, but it came out as $self->{miny}\n";
#		print "try to fix that by doing it with bigfloat (or should you have just rounded ?)\n";
#		my $bignewymin=Math::BigFloat->new(''.$nymin);
#		my $bigymin=Math::BigFloat->new(''.$self->{miny});
#		my $smalleroffset = $bignewymin - $bigymin;
#		if ($smalleroffset) {$self->translate(undef,$smalleroffset);}
#		if ($bignewymin ne $bigymin) {
#			print "     same old problem: $bignewymin ne $bigymin\n"
#			}
		}
	#print "ynewspec:",$self->{pathspec},"\n";
	}
sub normalizeX {
	my $self=shift;
	my $nxmin = shift;
	my $nxmax = shift;
	my $constrain = @_?shift:0;
	my $scalefactor=1;
	#print "norm X xoldspec:",$self->{pathspec},"\n";
	#$self->translate(-$self->{minx},-$self->{miny});
	#lets do this instead: assume the first point of the path is the reference point
	my $untranslatex=$self->{pathSegments}->[0]->{p1}->[0];
	my $untranslatey=$self->{pathSegments}->[0]->{p1}->[1];

	$self->translate(-$self->{pathSegments}->[0]->{p1}->[0],-$self->{pathSegments}->[0]->{p1}->[1]);
	my $xdiff=$self->{maxx} - $self->{minx};
	if (abs($xdiff) > 0.0000000000001) {
		$scalefactor = ($nxmax - $nxmin)/$xdiff;
		}
	my $newspec='';
	#print "xoldspec:$self->{pathspec}\n";
	foreach (@{$self->{pathSegmentSpecs}}) {
		#print "pathsegspec:",$_,"\n";
		my $segTypeLetter = substr($_,0,1);
		my @thesepoints= $self->extractPointsFromPathSpec($_);
		#if (!$constrain) {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')?eval(substr(($_->[0] * $scalefactor),0,25)).','.$_->[1]:$_} @thesepoints);}

		$newspec.=$segTypeLetter;
		my $first=1;
		foreach my $point (@thesepoints) {
			if (ref($point) eq 'ARRAY') {
				#my $newx=$point->[0] * $scalefactor * ($first && $segTypeLetter eq 'A'?(($self->{maxx} - $self->{minx})/(2*$point->[0])):1);
				my $newx=$point->[0] * $scalefactor ;
				my $newy=$point->[1];

				#don't recal reason for $first, and now it's causing problem
				#so I'll comment it out, and recreat a problem to be rediscoverd done the road
				#so that maybe I can get what I'm working on today working
				#if ($constrain && !($first && $segTypeLetter eq 'A')) {
				if ($constrain) {
					$newy*=$scalefactor;
					}
# SPEED ATTEMPTS
#				if (ref($newx) && !ref($point->[0])) {$newx=eval(substr($newx->bstr,0,25));}
#				if (ref($newy) && !ref($point->[1])) {$newy=eval(substr($newy->bstr,0,25));}
				if (ref($newx) && !ref($point->[0])) {$newx=0 + sprintf("%.20f",$newx->bstr);}
				if (ref($newy) && !ref($point->[1])) {$newy=0 + sprintf("%.20f",$newy->bstr);}
				$newspec.=$newx.','.$newy.',';
				}
			else {$newspec.=$point.',';}
			$first=0;
			}
		$newspec=substr($newspec,0,-1);#chop off last comma


		#if (!$constrain) {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')?($_->[0] * $scalefactor).','.$_->[1]:$_} @thesepoints);}
		##else             {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')?eval(substr(($_->[0] * $scalefactor),0,25)).','.eval(substr(($_->[1] * $scalefactor),0,25)):$_} @thesepoints);}
		#else             {$newspec.=$segTypeLetter . join(',',map {(ref($_) eq 'ARRAY')?($_->[0] * $scalefactor).','.($_->[1] * $scalefactor):$_} @thesepoints);}
		}
	#print "xnewspec:$newspec\n";
	$self->constructSegments($newspec);
	my $offset = $nxmin - $self->{minx};

	$self->translate($offset,$untranslatey);

	if ($nxmin ne $self->{minx}) {
	#	print "UH OH x: tried to set minx to $nxmin, but it came out as $self->{minx}\n";
#		print "try to fix that by doing it with bigfloat (or should you have just rounded ?)\n";
#		my $bignxmin=Math::BigFloat->new(''.$nxmin);
#		my $smalleroffset = $bignxmin - $self->{minx};
#		if ($smalleroffset) {$self->translate($smalleroffset,0);}
#		if ($nxmin ne $self->{minx}) {
#			print "     same old problem: $nxmin ne $self->{minx}\n"
#			}
		}
	#print "norm X newspec:",$self->{pathspec},"\n";

	}
sub translate {
	my $self = shift;
	my $xoff = shift;
	my $yoff = shift;
	$xoff ||= 0;
	$yoff ||= 0;
	if (!$xoff && !$yoff) {return;}
	my $xoffbig=$xoff;
	my $yoffbig=$yoff;
# SPEED ATTEMPTS
#	if (ref($xoffbig)) {$xoff = eval(substr($xoffbig->bstr(),0,25));}
#	if (ref($yoffbig)) {$yoff = eval(substr($yoffbig->bstr(),0,25));}
	if (ref($xoffbig)) {$xoff = 0 + sprintf("%.20f",$xoffbig->bstr());}
	if (ref($yoffbig)) {$yoff = 0 + sprintf("%.20f",$yoffbig->bstr());}

	#print "translate:xoff,yoff:$xoff,$yoff\ntranslate:oldspec:$self->{pathspec}\n";
	my $newspec='';
	foreach (@{$self->{pathSegmentSpecs}}) {
		if (substr($_,0,1) eq 'A') {
			my @pts=$self->extractPointsFromPathSpec($_);
			$newspec.=substr($_,0,1) . join(',',map {(ref($_) eq 'ARRAY')?($_->[0]).','.($_->[1]) : $_} @pts[0 .. $#pts - 1]);
# SPEED ATTEMPTS
#			$newspec.=','.eval(sprintf("%.9f",$pts[$#pts]->[0] + $xoff)).','.eval(sprintf("%.9f",$pts[$#pts]->[1] + $yoff));
			$newspec.=','.sprintf("%.9f",$pts[$#pts]->[0] + $xoff).','.sprintf("%.9f",$pts[$#pts]->[1] + $yoff);
			}
		else {
			#$newspec.=substr($_,0,1) . join(',',map {eval(sprintf("%.9f",$_->[0] + $xoff)).','.eval(sprintf("%.9f",$_->[1] + $yoff))} $self->extractPointsFromPathSpec($_));
			#$newspec.=substr($_,0,1) . join(',',map {eval(substr($_->[0] + $xoff,0,25)).','.eval(substr($_->[1] + $yoff,0,25))} $self->extractPointsFromPathSpec($_));
			$newspec.=substr($_,0,1) . join(',',map {($_->[0] + ((ref($_->[0])&&ref($xoff))?$xoffbig:$xoff)).','.($_->[1] + ((ref($_->[1])&&ref($yoff))?$yoffbig:$yoff))} $self->extractPointsFromPathSpec($_));
			}
		}
	$self->constructSegments($newspec);
	#print "translate:newspec:",$self->{pathspec},"\n";
	}
sub scale { # new, untested, copied from javascript version where it seems to work
	my $self = shift;
	my $xscale = shift;
	my $yscale = shift;

    # this stuff was wrong in javascript, but inertly so
    # but now I'm fixing it there and here in a way that might introduce weirdness,
    # since I'm not running and testing this at the moment
	if (!$xscale && $yscale) {$xscale=1;}
	if (!$yscale && $xscale) {$yscale=$xscale;}

	my $osp = "scale:oldspec:\n".$self->{pathspec}."\n";
	my $newspec='';
	for (my $i=0;$i<scalar(@{$self->{pathSegmentSpecs}});$i++) {
		my @pts=$self->extractPointsFromPathSpec($self->{pathSegmentSpecs}->[$i]);
		if (substr($self->{pathSegmentSpecs}->[$i],0,1) eq 'A') {
			$newspec.=substr($self->{pathSegmentSpecs}->[$i],0,1);

            # TODO: copy this expanded, partly fixed arc scale stuff back to js version

            # two radii (these scale, but should remain positive if either scale is negative)
            my $radii = shift @pts;
            # phi, large arc flag, sweep flag
            my $phi=shift @pts;
            my $lrgarc=shift @pts;
            my $sweep=shift @pts;
            # is this right? when one or other scale is negative (but not both)
            # flip sweep flag?
            if (($yscale < 0 && $xscale > 0) || ($yscale < 0 && $xscale > 0)) {
                if ($sweep) {$sweep=0;}
                else        {$sweep=1;}
                }

            # TODO: seems you should take phi into account here
            if ($phi ne 0) {die "not sure you want to simply scale radii when phi in effect - come address this";}

            $newspec.= ($radii->[0]*abs($xscale)) . ',' . ($radii->[1]*abs($yscale)) . ',';
            $newspec.= $phi.',';
            $newspec.= $lrgarc.',';
            $newspec.= $sweep;
			# end of arc point coords
			$newspec.=','.( ($pts[$#pts]->[0]) * $xscale).','.( ($pts[$#pts]->[1]) * $yscale);
			}
		else {
			$newspec.=substr($self->{pathSegmentSpecs}->[$i],0,1) . join(',',map { ($_->[0] * $xscale).','.($_->[1] * $yscale) } @pts);}
			}
	$self->constructSegments($newspec);
	print $osp,"scale:newspec:\n",$newspec,"\n";
	}
sub constructSegment {
	my $self = shift;
	my $segspec = shift;
	my $segspecprevious = shift;
	my $segTypeLetter = substr($segspec,0,1);
	my @lastpoints = $self->extractPointsFromPathSpec($segspecprevious);
	my @thesepoints= $self->extractPointsFromPathSpec($segspec);
	if    ($segTypeLetter eq 'C') {
		if ( # check for degenerate curve
			($lastpoints[$#lastpoints]->[0] eq $thesepoints[0]->[0] && $lastpoints[$#lastpoints]->[0] eq $thesepoints[1]->[0] && $lastpoints[$#lastpoints]->[0] eq $thesepoints[2]->[0]) ||
			($lastpoints[$#lastpoints]->[1] eq $thesepoints[0]->[1] && $lastpoints[$#lastpoints]->[1] eq $thesepoints[1]->[1] && $lastpoints[$#lastpoints]->[1] eq $thesepoints[2]->[1])
			) {
			my $ret=Math::MikePath::LineSegment->new($lastpoints[$#lastpoints],$thesepoints[2],$self->{precision},$self->{isLite});
			#print "line instead of curve: $ret->{minx},$ret->{miny},$ret->{maxx},$ret->{maxy}\n";
			#print "    x at 0: ",$ret->point(0)->[0]," y at 0: ",$ret->point(0)->[1],"\n";
			#print "    x at 1: ",$ret->point(1)->[0]," y at 1: ",$ret->point(1)->[1],"\n";
			#print "    f(0) : ",($ret->f(0))[0],"\n";
			#print "    F(0) : ",($ret->F(0))[0],"\n";
			return $ret;
			}
		else {
			return Math::MikePath::BezierCubicSegment->new($lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});
			}
		}
	elsif ($segTypeLetter eq 'L') {return Math::MikePath::LineSegment->new($lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}
	elsif ($segTypeLetter eq 'H') {return Math::MikePath::LineSegment->new($lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}
	elsif ($segTypeLetter eq 'V') {return Math::MikePath::LineSegment->new($lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}
#	elsif ($segTypeLetter eq 'H') {
#        @thesepoints = [$thesepoints[0],$lastpoints[$#lastpoints][1]];
#	    return Math::MikePath::LineSegment->new($lastpoints[$#lastpoints],$thesepoints[0],$self->{precision});
#        }
#	elsif ($segTypeLetter eq 'V') {
#        @thesepoints = [$lastpoints[$#lastpoints][0],$thesepoints[0]];
#	    return Math::MikePath::LineSegment->new($lastpoints[$#lastpoints],$thesepoints[0],$self->{precision});
#        }
   	elsif ($segTypeLetter eq 'M') {return Math::MikePath::MoveTo->new(     $lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}
	elsif ($segTypeLetter eq 'Z') {return Math::MikePath::ClosePath->new(  $lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}
	elsif ($segTypeLetter eq 'z') {return Math::MikePath::ClosePath->new(  $lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}
	elsif ($segTypeLetter eq 'A') {return Math::MikePath::EllipticalArc->new($lastpoints[$#lastpoints],@thesepoints,$self->{precision},$self->{isLite});}

	}
sub extractPointsFromPathSpec {
	my $self=shift;
	my $segspec = shift;
	#print "EXTRACTING FROM:$segspec\n";
	my @ret;
	my $segTypeLetter = substr($segspec,0,1);
	if    ($segTypeLetter eq 'M') {push @ret , [split(/[ ,]+/,substr($segspec,1))]}
	elsif ($segTypeLetter eq 'L') {push @ret , [split(/[ ,]+/,substr($segspec,1))]}
	elsif ($segTypeLetter eq 'H') {push @ret , [split(/[ ,]+/,substr($segspec,1))]}
	elsif ($segTypeLetter eq 'V') {push @ret , [split(/[ ,]+/,substr($segspec,1))]}
#	elsif ($segTypeLetter eq 'H') {push @ret , [substr($segspec,1)]}
#	elsif ($segTypeLetter eq 'V') {push @ret , [substr($segspec,1)]}
	#elsif ($segTypeLetter eq 'Z') {push @ret , [$self->{pathSegments}->[0]->{p1}->[0],$self->{pathSegments}->[0]->{p1}->[1]]}
	#elsif ($segTypeLetter eq 'z') {push @ret , [$self->{pathSegments}->[0]->{p1}->[0],$self->{pathSegments}->[0]->{p1}->[1]]}
	#starting to support multipath paths (multiple Ms and maybe Zs)
	#coords parsed here for Z command were injected upstream, but shouldn't appear in representations of SVG path specs
	elsif ($segTypeLetter eq 'Z') {push @ret , [split(/[ ,]+/,substr($segspec,1))]}
	elsif ($segTypeLetter eq 'z') {push @ret , [split(/[ ,]+/,substr($segspec,1))]}
	elsif ($segTypeLetter eq 'C') {my ($cp1x,$cp1y,$cp2x,$cp2y,$px,$py) = split(/[ ,]+/,substr($segspec,1));push @ret , ([$cp1x,$cp1y],[$cp2x,$cp2y],[$px,$py])}
	elsif ($segTypeLetter eq 'A') {my ($rx,$ry,$phi,$lrgarc,$sweep,$px,$py) = split(/[ ,]+/,substr($segspec,1));push @ret , ([$rx,$ry],$phi,$lrgarc,$sweep,[$px,$py])}
	for (my $i=0;$i<@ret;$i++) {
		if (ref($ret[$i]) eq 'ARRAY') {
			foreach my $num (@{$ret[$i]}) {
				my $sigdigits=0;
				if ($num =~ /^-?([0-9]+\.[0-9]+[1-9])0*$/i) {$sigdigits=length($1) - 1;}
				if ($num =~ /^-?0\.0*([1-9][0-9]+?[1-9])0*$/) {$sigdigits=length($1);} #small numbers that might fit in perl float
				if ($num =~ /^-?([1-9][0-9]+?[1-9])0*\.?0*$/) {$sigdigits=length($1);} #big numbers that might fit in perl float
				if ($sigdigits>15) {
					#print "big number in path spec: $num making big float\n";
					#cluck "big number in path spec: $num making big float\n";
					#$num=Math::BigFloat->new(''.$num);
					#warn "no, not making big!\n";
# SPEED ATTEMPTS
#					$num=eval($num);
					}
				else {
					#print "$num will be set to " , eval($num),"\n";
# SPEED ATTEMPTS
#					$num=eval($num);
					}
				#@{$ret[$i]}=map {if (length($_)>19) {print "\nlong one: $_\n";};length($_)>19?Math::BigFloat->new(''.$_):$_} @{$ret[$i]};}
				}
			}
		}
	return @ret;
	}

sub getFeet {
	my $self = shift;
	my $x= shift;
	my $y= shift;
	my @feet;
#	foreach my $selfseg (@{$self->{pathSegments}}) {
	for (my $i=0;$i< scalar(@{$self->{pathSegments}});$i++) {
		my @f=$self->{pathSegments}->[$i]->getFeet($x,$y);
		if (scalar(@f)) {
			for (my $j=0;$j<scalar(@f);$j++) {
				$f[$j]->[2] = $self->getPathThetaAtSegTheta($i,$f[$j]->[2]);
				}
			push(@feet,@f);
			}
		}
	#if (!scalar(@feet)) {
	#	#print "NOOOO FEET for [$x,$y]\n";
	#	}
	return @feet;
	}
sub getIntersections {
	my $self = shift;
	my $other= shift;
	my $wantThetas = scalar(@_) ? shift:0;
	my $wantNativeThetas = scalar(@_) ? shift:0;
	my @intersects=();
	for (my $i=0;$i<@{$self->{pathSegments}};$i++) {
		foreach my $otherseg (@{$other->{pathSegments}}) {
			push(@intersects,map {
			 ($wantThetas && !$wantNativeThetas)
			 ? $self->getPathThetaAtSegTheta($i,$_)
			 : $_
			 } getSegSegIntersects($self->{pathSegments}->[$i],$otherseg,$wantThetas));
			}
		}
	return @intersects;
	}
sub getSegSegIntersects {
	my $seg1=shift;
	my $seg2=shift;
	my $wantThetas = scalar(@_) ? shift:0;
	my $refstrings = ref($seg1).'--'.ref($seg2);
	my @ret;

	if (($refstrings=~/LineSegment/ || $refstrings=~/ClosePath/) && $refstrings=~/BezierCubicSegment/) {
		my $line;
		my $curve;
		my $lineIsSelf;
		if ($refstrings=~/LineSegment.*?--/ || $refstrings=~/ClosePath.*?--/) {$lineIsSelf=1;($line,$curve) = ($seg1,$seg2);}
		else                                 {$lineIsSelf=0;($line,$curve) = ($seg2,$seg1);}

		# t^3 + [(F-mB)/(E-mA)]t^2 + [(G-mC)/(E-mA)]t + [(H-mD-x0)/(E-mA)] = 0 , I hope

        # Yeah, but you should have noted why. How'd you work that up again?
        # You took the equation of a line y = mx + b
        # Used the Bezier's y(t) and x(t) for y and x, used the line's m and b,
        # then arranged it like this: y - mx - b = 0
        # and the left side comes out as a cubic polynomial in t that we can
        # use the good old cubic solver for. Sounds right.

		my @thetas;
		if ($line->{m} eq 'inf' || $line->{m} eq '-inf') {
			#then I hope this is right
			@thetas = $curve->solveXforTheta($line->{maxx});
			foreach my $t (@thetas) {
				my $y = $curve->bezierEvalYofT($t);
				#print "an y: $y\n";
				#print "line maxy: $line->{maxy}\n";
				#if ($y <= $line->{maxy} + $line->{precision} &&
				#	$y >= $line->{miny} - $line->{precision}) {
				if (($y < $line->{maxy} || $y eq $line->{maxy}) &&
					($y > $line->{miny} || $y eq $line->{miny})) {
					if ($wantThetas) {
						if (!$lineIsSelf) {push(@ret,$t);}
						else {push(@ret,$line->solveYforTheta($y));}
						}
					else {push(@ret,[$line->{maxx},$y]);}
					}
				}
			}
		elsif ($line->{m} eq 0) {
			#warn "zero slope, snap special";
			if ($wantThetas && !$lineIsSelf) {
                my @ths = $curve->solveYforTheta($line->{p1}->[1]);
				push(@ret, grep {my $p=$curve->point($_); ($p->[0] < $line->{maxx} || $p->[0] eq $line->{maxx}) && ($p->[0] > $line->{minx} || $p->[0] eq $line->{minx})} @ths);
                }
            else {
                # do F(y) to get possible x vals from bezier
    			my @xs = $curve->F($line->{p1}->[1]);
    			# then filter to what's actually within line segment bounds
                # also, the reason this zero slope thing is handled seperately
                # is that we make sure the resulting y values are exactly the horizontal line's y value
                if ($wantThetas) {
                    push(@ret,map { $line->solveXforTheta($_)} @xs);
                    }
                else {
                    my @intersections = map {[$_,$line->{p1}->[1]]} grep { ($_ < $line->{maxx} || $_ eq $line->{maxx}) && ($_ > $line->{minx} || $_ eq $line->{minx})} @xs;
    				push(@ret,@intersections);
                    }
				}
			}
		else {
		    # think what you did here was set the bezier's x(t) function equal
		    # to the line's ... hmm. No.
		    # you might have started with Bezy(t) = Line(y(t)) ??? ,nuhhhh, hmm.
		    # what did you do? where are the notes?

			@thetas = &cubicformula(
			    ($curve->{F}-$line->{m}*$curve->{B})            / ($curve->{E}-$line->{m}*$curve->{A}),
			    ($curve->{G}-$line->{m}*$curve->{C})            / ($curve->{E}-$line->{m}*$curve->{A}),
			    ($curve->{H}-$line->{m}*$curve->{D}-$line->{b}) / ($curve->{E}-$line->{m}*$curve->{A}),
			    1);

			#@thetas = &cubicformula(($curve->{F_Big}-$line->{m}*$curve->{B_Big})/($curve->{E_Big}-$line->{m}*$curve->{A_Big}),($curve->{G_Big}-$line->{m}*$curve->{C_Big})/($curve->{E_Big}-$line->{m}*$curve->{A_Big}),($curve->{H_Big}-$line->{m}*$curve->{D_Big}-$line->{b})/($curve->{E_Big}-$line->{m}*$curve->{A_Big}),1);
			#print "uh thetas:",join(",",@thetas),"  and line slope:$line->{m}\n";
			@thetas = sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)}  @thetas;
			#print "uh _Big thetas:",join(",",@thetas),"\n";
			#@thetas = sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} @thetas;
			foreach my $t (@thetas) {
				my $x = $curve->bezierEvalXofT($t);
				#print "an x: $x\n";
				#print "line maxx: $line->{maxx}\n";
				#if ($x <= $line->{maxx} + $line->{precision} &&
				#	$x >= $line->{minx} - $line->{precision}) {
				if (($x < $line->{maxx} || $x eq $line->{maxx}) &&
					($x > $line->{minx} || $x eq $line->{minx})) {
					if ($wantThetas) {
						if (!$lineIsSelf) {push(@ret,$t);}
						else {push(@ret,$line->solveXforTheta($x));}
						}
					else {push(@ret,[$x,$curve->bezierEvalYofT($t)]);}
					}
				}
			}
		}
	elsif ($refstrings=~/(LineSegment|ClosePath).*?--/ && $refstrings=~/--.*?(LineSegment|ClosePath)/) {
#		# m1x + b1 = m2x + b2
#		# (m1-m2)x = b2-b1
#		# x = (b2-b1)/(m1-m2)
#		my $x;
#		my $dm = ($seg1->{m}-$seg2->{m});
#		if    (($seg1->{m} eq 'inf' || $seg1->{m} eq '-inf') && $seg2->{m} ne 'inf' && $seg2->{m} ne '-inf') {$x = $seg1->{p1}->[0];}
#		elsif (($seg2->{m} eq 'inf' || $seg2->{m} eq '-inf') && $seg1->{m} ne 'inf' && $seg1->{m} ne '-inf') {$x = $seg2->{p1}->[0];}
#		elsif (abs($dm) > 0.000000000001) {$x=($seg2->{b}-$seg1->{b})/$dm;}
#		#my $mostprec = ($seg1->{precision}<$seg2->{precision})?$seg1->{precision}:$seg2->{precision};
#		if (defined($x) &&
#			#$x <= $seg1->{maxx} + $mostprec &&
#			#$x >= $seg1->{minx} - $mostprec &&
#			#$x <= $seg2->{maxx} + $mostprec &&
#			#$x >= $seg2->{minx} - $mostprec) {
#			($x < $seg1->{maxx} || $x eq $seg1->{maxx} ) &&
#			($x > $seg1->{minx} || $x eq $seg1->{minx} ) &&
#			($x < $seg2->{maxx} || $x eq $seg2->{maxx} ) &&
#			($x > $seg2->{minx} || $x eq $seg2->{minx} ) ) {
#			if ($wantThetas) {push(@ret,($seg1->{m} eq 'inf')?$seg1->solveYforTheta($seg2->f($x)):$seg1->solveXforTheta($x));}
#			else {push(@ret,[$x,($seg1->{m} eq 'inf' || $seg1->{m} eq '-inf')?$seg2->f($x):$seg1->f($x)]);}
#			}



## start new stuff adapted from best working seg_line_intersection() function from elsewhere
		my $segsegret;

		my $x1= $seg1->{p1}->[0];my $y1= $seg1->{p1}->[1];
		my $x2= $seg1->{p2}->[0];my $y2= $seg1->{p2}->[1];
		my $u1=$seg2->{p1}->[0];my $v1=$seg2->{p1}->[1];
		my $u2=$seg2->{p2}->[0];my $v2=$seg2->{p2}->[1];
		my $m1 = (($x2 - $x1)==0)?'Inf':($y2 - $y1)/($x2 - $x1);
		my $m2 = (($u2 - $u1)==0)?'Inf':($v2 - $v1)/($u2 - $u1);

		my $b1;
		my $b2;

		my  $xi;
		my $dm = $m1 - $m2;
		if    ($m1 eq 'Inf' && $m2 ne 'Inf') {$xi = $x1;$b2 = $v1 - ($m2 * $u1);}
		elsif ($m2 eq 'Inf' && $m1 ne 'Inf') {$xi = $u1;$b1 = $y1 - ($m1 * $x1);}
		elsif (abs($dm) > 0.000000000001) {
			$b1 = $y1 - ($m1 * $x1);
			$b2 = $v1 - ($m2 * $u1);
			$xi=($b2-$b1)/$dm;
			}
		#print "M1:$m1 , M2:$m2, DM:$dm, XI: $xi\n";
		my @lowhiu=($u2>$u1)?($u1,$u2):($u2,$u1);
		if ($m1 ne 'Inf') {
			my @lowhix=($x2>$x1)?($x1,$x2):($x2,$x1);
			if ($m2 eq 'Inf' &&   ($u2<$lowhix[0] || $u2>$lowhix[1]) ) {
				#nothing - because no intersection
				}
			elsif (
				($xi || $xi eq 0) &&
				($xi < $lowhix[1] || $xi eq $lowhix[1]) &&
				($xi > $lowhix[0] || $xi eq $lowhix[0]) &&
				($xi < $lowhiu[1] || $xi eq $lowhiu[1]) &&
				($xi > $lowhiu[0] || $xi eq $lowhiu[0])
				) {
				my $y=($m1*$xi)+$b1;
				my @lowhiv=($v2>$v1)?($v1,$v2):($v2,$v1);
				if ($m2 eq 'Inf' && # in this case we set $xi above even though thre might not be an intersection. If $y not in range of other seg's y extremes, no intersection
					($y<$lowhiv[0] || $y>$lowhiv[1])
					) {
					#nothing
					}
				else {
					$segsegret = [$xi,$y];
					}
				}
			}
		elsif ($m2 ne 'Inf'
			&& (
				($x1 > $lowhiu[0] && $x1 < $lowhiu[1])
				||
				($x1 eq $lowhiu[0] && $x1 eq $lowhiu[1])
				)
			) {
			my @lowhiy=($y2>$y1)?($y1,$y2):($y2,$y1);
			my @lowhiv=($v2>$v1)?($v1,$v2):($v2,$v1);
			my $yi = ($m2*$xi)+$b2;
			if (($yi || $yi eq 0) &&
				($yi < $lowhiy[1] || $yi eq $lowhiy[1]) &&
				($yi > $lowhiy[0] || $yi eq $lowhiy[0]) &&
				($yi < $lowhiv[1] || $yi eq $lowhiv[1]) &&
				($yi > $lowhiv[0] || $yi eq $lowhiv[0])
				) {
				$segsegret=[$xi,$yi];
				}
			}

		if (defined($segsegret)) {
			if ($wantThetas) {push(@ret,($m1 eq 'Inf')?$seg1->solveYforTheta($segsegret->[1]):$seg1->solveXforTheta($segsegret->[0]));}
			else {push(@ret,$segsegret);}
			}
### end new stuff


		}
	elsif (($refstrings=~/LineSegment/ || $refstrings=~/ClosePath/) && $refstrings=~/EllipticalArc/) {
		my $line;
		my $arc;
		my $lineIsSelf;
		if ($refstrings=~/LineSegment.*?--/ || $refstrings=~/ClosePath.*?--/) {$lineIsSelf=1;($line,$arc)=($seg1,$seg2);}
		else                                 {$lineIsSelf=0;($line,$arc)=($seg2,$seg1);}
		my @intersections;
		my $x1=$line->{p1}->[0];
		my $y1=$line->{p1}->[1];
		my $x2=$line->{p2}->[0];
		my $y2=$line->{p2}->[1];
		#print "line-arc intercepts:\n    line : [$x1,$y1] , [$x2,$y2]\n";
		$x1-=$arc->{cx};
		$y1-=$arc->{cy};
		$x2-=$arc->{cx};
		$y2-=$arc->{cy};
		#print "if ($line->{maxx}>$arc->{minx} && $line->{minx}<$arc->{maxx} && $line->{maxy}>$arc->{miny} && $line->{miny}<$arc->{maxy}) {\n";
		#if ($line->{maxx}>$arc->{minx} && $line->{minx}<$arc->{maxx} && $line->{maxy}>$arc->{miny} && $line->{miny}<$arc->{maxy}) {
		if (
			($line->{maxx}>$arc->{minx}  || $line->{maxx} eq $arc->{minx})&&
			($line->{minx}<$arc->{maxx}  || $line->{minx} eq $arc->{maxx})&&
			($line->{maxy}>$arc->{miny} || $line->{maxy} eq $arc->{miny}) &&
			($line->{miny}<$arc->{maxy}  || $line->{miny} eq $arc->{maxy})
			) {
			my $rot_line_p1 = _rotate2d([0,0],[$x1,$y1],-1 * $arc->{phi_radians});
			my $rot_line_p2 = _rotate2d([0,0],[$x2,$y2],-1 * $arc->{phi_radians});

			if (abs(($rot_line_p2->[0]-$rot_line_p1->[0]))>0.000001) { # had this at > 0.1, but that seems weak. make it smaller and see if it's a problem
				my $rot_line_slope=($rot_line_p2->[1]-$rot_line_p1->[1])/($rot_line_p2->[0]-$rot_line_p1->[0]);

				#print "phi : $arc->{phi_radians}\nsin(phi)/cos(phi):",sin($arc->{phi_radians}),"/",cos($arc->{phi_radians}),"\n";
				#print "rot line: [$rot_line_p1->[0],$rot_line_p1->[1]],[$rot_line_p2->[0],$rot_line_p2->[1]]\n";

				my $a = (($rot_line_slope)**2/$arc->{ry}**2) + 1/$arc->{rx}**2;
				my $b = ( 2 * ($rot_line_slope) * ($rot_line_p1->[1] - ($rot_line_slope)*$rot_line_p1->[0]))/$arc->{ry}**2;
				my $c =(($rot_line_p1->[1] - ($rot_line_slope)*$rot_line_p1->[0])**2 / $arc->{ry}**2 ) - 1;
				#print "quad coeffs: $a, $b, $c\n";
				my @xs = &quadraticformula($a,$b,$c,1);
				#print "solution(s) from quad form:",join(",",@xs),"\n";
				for (my $i=0;$i<@xs;$i++) {
					my $y=$rot_line_slope * $xs[$i] + ($rot_line_p1->[1] - $rot_line_slope * $rot_line_p1->[0]); #line formula
					push(@intersections,[$xs[$i],$y]);
					}
				}
			else {#vertical line - use ellipse formula to get points
				#print "VERT (min/max x: $line->{minx},$line->{maxx}) line [$line->{p1}->[0],$line->{p1}->[1] , $line->{p2}->[0],$line->{p2}->[1]] doin ok\n";
				my $y=sqrt($arc->{ry}**2 * (1 - ($x1**2)/($arc->{rx}**2)));#vertical line. use ellipse formula to get the +/- y vals
				push(@intersections,[$x1,$y],[$x1,-$y]);
				#print "   ints even: [ $x1 , +/- $y]\n";
				}
			#for (my $i=0;$i<@intersections;$i++) {
			#	print "centered, unrotated intersection $i: [$intersections[$i]->[0],$intersections[$i]->[1]]\n";
			#	}
			for (my $i=0;$i<@intersections;$i++) {
				# there was an interesting point of failure here due to floating point error in addition
				# when adding cx back to intersection's x, sometimes the result would get an extra 0.000000000000001 (or so),
				# when really it should have been equal to the vert line's minx/maxx
				# so the equality tests later would fail when they shouldn't have.
				# So here I'm adjusting significant digits for the addition with a sprintf(),
				# and then evaling the result to clip off any trailing zeros.
				# (So four simple lines of code become a jumbled ugly eight.)
				# (plus eight more to comment on it all)
				my $h=sqrt($intersections[$i]->[0]**2 + $intersections[$i]->[1]**2);
				$intersections[$i] = _rotate2d([0,0],$intersections[$i],$arc->{phi_radians});
				my $sigcnt1=length(($arc->{cx}             =~/\.([0-9]*)/g)[0]) + length(($arc->{cx}             =~/([0-9]*)[0-9]\./g)[0]);
				my $sigcnt2=length(($intersections[$i]->[0]=~/\.([0-9]*)/g)[0]) + length(($intersections[$i]->[0]=~/([0-9]*)[0-9]\./g)[0]);
				#print "add thing: $intersections[$i]->[0]+=$arc->{cx} = ";
				$intersections[$i]->[0]+=$arc->{cx};
				#print "$intersections[$i]->[0]  then  ";
				my $sigcnt3=length(($intersections[$i]->[0]=~/\.([0-9]*)/g)[0]);
				$intersections[$i]->[0]=0 + sprintf("%.".(sort {$b<=>$a} ($sigcnt1,$sigcnt2))[0]."f",$intersections[$i]->[0]);
				#print "$intersections[$i]->[0]\n";
				$intersections[$i]->[1]+=$arc->{cy};
				}

			#Now check to see of those intersections are within bounds and within sweep

#if ( 0 &&   #degug thing
#	! grep {
#					($_->[0] < $line->{maxx} || abs($_->[0] - $line->{maxx}) < $line->{precision}) &&
#					($_->[0] > $line->{minx} || abs($_->[0] - $line->{minx}) < $line->{precision}) &&
#					($_->[1] < $line->{maxy} || abs($_->[1] - $line->{maxy}) < $line->{precision}) &&
#					($_->[1] > $line->{miny} || abs($_->[1] - $line->{miny}) < $line->{precision})
#
#				} @intersections
#	) {
#	map {
#	print "			($_->[0] < $line->{maxx} || abs($_->[0] - $line->{maxx}) < $line->{precision}) && \n";
#	print "			($_->[0] > $line->{minx} || abs($_->[0] - $line->{minx}) < $line->{precision}) && \n";
#	print "			($_->[1] < $line->{maxy} || abs($_->[1] - $line->{maxy}) < $line->{precision}) && \n";
#	print "			($_->[1] > $line->{miny} || abs($_->[1] - $line->{miny}) < $line->{precision})\n";
#	 } @intersections;
#
#	}
				@intersections = grep {
				#$_->[0] <= $line->{maxx} + $line->{precision} &&
				#$_->[0] >= $line->{minx} - $line->{precision} &&
				#$_->[1] <= $line->{maxy} + $line->{precision} &&
				#$_->[1] >= $line->{miny} - $line->{precision}

				#($_->[0] < $line->{maxx} || $_->[0] eq $line->{maxx}) &&
				#($_->[0] > $line->{minx} || $_->[0] eq $line->{minx}) &&
				#($_->[1] < $line->{maxy} || $_->[1] eq $line->{maxy}) &&
				#($_->[1] > $line->{miny} || $_->[1] eq $line->{miny})

				($_->[0] < $line->{maxx} || abs($_->[0] - $line->{maxx}) < $line->{precision}) &&
				($_->[0] > $line->{minx} || abs($_->[0] - $line->{minx}) < $line->{precision}) &&
				($_->[1] < $line->{maxy} || abs($_->[1] - $line->{maxy}) < $line->{precision}) &&
				($_->[1] > $line->{miny} || abs($_->[1] - $line->{miny}) < $line->{precision})


				} @intersections;

			my $leg1;
			my $leg2;
			if ($arc->{large_arc_flag}==0) {
				if ($arc->{sweep_flag} == 0) {
					$leg1=[[$arc->{cx},$arc->{cy}],[$arc->{p1}->[0],$arc->{p1}->[1]]];
					$leg2=[[$arc->{cx},$arc->{cy}],[$arc->{p2}->[0],$arc->{p2}->[1]]];
					}
				else {
					$leg1=[[$arc->{cx},$arc->{cy}],[$arc->{p2}->[0],$arc->{p2}->[1]]];
					$leg2=[[$arc->{cx},$arc->{cy}],[$arc->{p1}->[0],$arc->{p1}->[1]]];
					}
				}
			else {
				if ($arc->{sweep_flag} == 0) {
					$leg1=[[$arc->{cx},$arc->{cy}],[$arc->{p2}->[0],$arc->{p2}->[1]]];
					$leg2=[[$arc->{cx},$arc->{cy}],[$arc->{p1}->[0],$arc->{p1}->[1]]];
					}
				else {
					$leg1=[[$arc->{cx},$arc->{cy}],[$arc->{p1}->[0],$arc->{p1}->[1]]];
					$leg2=[[$arc->{cx},$arc->{cy}],[$arc->{p2}->[0],$arc->{p2}->[1]]];
					}
				}
			@intersections = grep {
			                          ( $arc->{large_arc_flag} && !$arc->isWithinSweep($_,$leg1,$leg2))
			                       || (!$arc->{large_arc_flag} &&  $arc->isWithinSweep($_,$leg1,$leg2))
			                 } @intersections;

			if ($wantThetas) {
				foreach my $int (@intersections) {
					if ($lineIsSelf) {
						push(@ret,($line->{m} eq 'inf')?$line->solveYforTheta($int->[1]):$line->solveXforTheta($int->[0]));
						}
					else {
						my @allArcThetas=$arc->solveXforTheta($int->[0]);
                        #warn "CIR--LINE allarcthetas: ",join(',',@allArcThetas),"\n";
						foreach my $t (@allArcThetas) {
							my $tp=$arc->point($t);
							if (abs($tp->[1] - $int->[1]) < 0.0000000001) {push(@ret,$t);}
							#man.
							}
						}
					}
				}
			else {
				push(@ret,@intersections);
				}
			#print "got ",scalar(@intersections)," line-arc intersections\n";
			#print "  it/they is/are: \n  ",join("\n  ",map {"[$_->[0],$_->[1]]"} @intersections),"\n";

			}
		}
    # circle-circle special (but common) case
	elsif (   $refstrings=~/EllipticalArc.*?--/ && $refstrings=~/--.*?EllipticalArc/
	       && $seg1->{rx} eq $seg1->{ry}
	       && $seg2->{rx} eq $seg2->{ry}
	      ) {

		my $arc1 = $seg1;
		my $arc2 = $seg2;

        # Paul Bourke
        # http://paulbourke.net/geometry/circlesphere/

        my $center_dist = sqrt(($arc2->{cx}-$arc1->{cx})**2 + ($arc2->{cy}-$arc1->{cy})**2);
        my $radius_sum  = $arc1->{rx} + $arc2->{rx};
        next if ($center_dist > $radius_sum);
        my $radius_diff  = $arc1->{rx} - $arc2->{rx};
        next if ($center_dist < abs(my $radius_diff));

        if ($center_dist eq 0 && $arc1->{rx} eq $arc2->{rx}) {
            die "crap. come figure out infinite intersection solution for circle-circle arc intersection";
            }

        my $a = ($arc1->{rx}**2 - $arc2->{rx}**2 + $center_dist**2)/(2*$center_dist);
        my $h = sqrt($arc1->{rx}**2 - $a**2);
        my $x2 = $arc1->{cx} + $a*($arc2->{cx} - $arc1->{cx})/$center_dist;
        my $y2 = $arc1->{cy} + $a*($arc2->{cy} - $arc1->{cy})/$center_dist;

        my @intersections;
        push @intersections, [$x2 + $h * ($arc2->{cy} - $arc1->{cy}) / $center_dist,
                              $y2 - $h * ($arc2->{cx} - $arc1->{cx}) / $center_dist];
        # unless the at-one-point intersect case, figure the other intersection
        unless (   $center_dist eq $arc1->{rx} + $arc2->{rx}
                || $center_dist eq $arc1->{rx} - $arc2->{rx}
               ) {
        push @intersections, [$x2 - $h * ($arc2->{cy} - $arc1->{cy}) / $center_dist,
                              $y2 + $h * ($arc2->{cx} - $arc1->{cx}) / $center_dist];
            }

        # now, see if those are within arc sweep
        # hopefully just copy paste from arc-line code...

        #warn "  circ--circ pre filter\n  ",join("\n  ",map {"[$_->[0],$_->[1]]"} @intersections),"\n";

        if (scalar(@intersections) > 0) {
			my $leg1;
			my $leg2;

            # copy pasted modded twice, once for each arc in this case
			if ($arc1->{large_arc_flag}==0) {
				if ($arc1->{sweep_flag} == 0) {
					$leg1=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p1}->[0],$arc1->{p1}->[1]]];
					$leg2=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p2}->[0],$arc1->{p2}->[1]]];
					}
				else {
					$leg1=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p2}->[0],$arc1->{p2}->[1]]];
					$leg2=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p1}->[0],$arc1->{p1}->[1]]];
					}
				}
			else {
				if ($arc1->{sweep_flag} == 0) {
					$leg1=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p2}->[0],$arc1->{p2}->[1]]];
					$leg2=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p1}->[0],$arc1->{p1}->[1]]];
					}
				else {
					$leg1=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p1}->[0],$arc1->{p1}->[1]]];
					$leg2=[[$arc1->{cx},$arc1->{cy}],[$arc1->{p2}->[0],$arc1->{p2}->[1]]];
					}
				}
			@intersections = grep {
                (     $arc1->{large_arc_flag} && !$arc1->isWithinSweep($_,$leg1,$leg2))
			     || (!$arc1->{large_arc_flag} &&  $arc1->isWithinSweep($_,$leg1,$leg2))
                } @intersections;

            #warn "  circ--circ post arc1 filter\n  ",join("\n  ",map {"[$_->[0],$_->[1]]"} @intersections),"\n";

            # now for other arc
			if ($arc2->{large_arc_flag}==0) {
				if ($arc2->{sweep_flag} == 0) {
					$leg1=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p1}->[0],$arc2->{p1}->[1]]];
					$leg2=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p2}->[0],$arc2->{p2}->[1]]];
					}
				else {
					$leg1=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p2}->[0],$arc2->{p2}->[1]]];
					$leg2=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p1}->[0],$arc2->{p1}->[1]]];
					}
				}
			else {
				if ($arc2->{sweep_flag} == 0) {
					$leg1=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p2}->[0],$arc2->{p2}->[1]]];
					$leg2=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p1}->[0],$arc2->{p1}->[1]]];
					}
				else {
					$leg1=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p1}->[0],$arc2->{p1}->[1]]];
					$leg2=[[$arc2->{cx},$arc2->{cy}],[$arc2->{p2}->[0],$arc2->{p2}->[1]]];
					}
				}
			@intersections = grep {
                (     $arc2->{large_arc_flag} && !$arc2->isWithinSweep($_,$leg1,$leg2))
			     || (!$arc2->{large_arc_flag} &&  $arc2->isWithinSweep($_,$leg1,$leg2))
                } @intersections;


			#warn "got ",scalar(@intersections)," circle_arc-circle_arc intersections\n";
			#warn "  it/they is/are: \n  ",join("\n  ",map {"[$_->[0],$_->[1]]"} @intersections),"\n";

			if ($wantThetas) {
				foreach my $int (@intersections) {
					my @allArcThetas=$arc1->solveXforTheta($int->[0]);
                    #warn "CIR--CIR allarcthetas: ",join(',',@allArcThetas),"\n";
					foreach my $t (@allArcThetas) {
						my $tp=$arc1->point($t);
						if (abs($tp->[1] - $int->[1]) < 0.0000000001) {push(@ret,$t);}
						#man. # why man? were you complaining about doing a tolerance thing here? whatever man!
						}
					}
				}
			else {
				push(@ret,@intersections);
				}
			#print "got ",scalar(@intersections)," circle_arc-circle_arc intersections\n";
			#print "  it/they is/are: \n  ",join("\n  ",map {"[$_->[0],$_->[1]]"} @intersections),"\n";
            }

        }

    # general ellipse-ellipse case
	elsif (   $refstrings=~/EllipticalArc.*?--/ && $refstrings=~/--.*?EllipticalArc/ ) {
        # will be kinda hairy. probably needs quartic solver, unless
        # you can find shortcut due to working with arcs and not full ellipses, in general
        # or unless you cook up a quick (to code) rootfinding appoach
        die "elliptical arc--elliptical arc intersection not handled yet (when both aren't circular arcs)";
        }

    # yeah bez--bez isn't here! you thought you had it but you don't
    # but now you might be able to figure it, based on breaking bezs down to y(t(x)) sub functions
    # -- you'd solve intersection of those. Gotta work all that up though to see if that works.
    # You'll also be in position to do offset bez and offset intersections then, I think.
    # But need serious work sessions for that. Better space. More peace, for longer.

	return @ret;
	}

our $BigFloatOneHalf = Math::BigFloat->new('0.5');
our $BigFloatTen     = Math::BigFloat->new('10');
sub bigsqrt {
	#because the sqrt and root functions in Math::BigFloat sometimes fail
	#Wikipedia says:
	#sqrt(x) = 10**(1/2 * log_10(x))
    # doing similar thing in QuadraticFormula.pm. see comments there
	return $BigFloatTen->copy()->bpow($BigFloatOneHalf->copy()->bmul($_[0]->copy()->blog(10)),25);
	}

sub dimensionalStepFromTheta {
	my $self=shift;
	#print " in dimstep ref:",ref($self),"\n";

	my $dim=shift;
	my $theta=shift;
	my $direction=scalar(@_)?shift:1; # 1 or 0

	my $findnexttheta = sub {
		my $ret;
		#print " in sub dimstep ref:",ref($self),"\n";
		my $pt_last = $self->point($theta);
		if (!ref($_[0])) {
			my $pt_new  = $self->point($_[0]);
			$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2);
			#warn "$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2) = $ret\n";
			}
		else {
			#warn "I don't think you want to be here - not sure if this mess is debugged.\n";
			my $pt_new  = $self->point($_[0]);
			#print "using BigFloat - \n";
			my $dx=(ref($pt_new->[0]))?$pt_new->[0]->copy()->bsub($pt_last->[0]):$pt_new->[0] - $pt_last->[0];
			my $dxsqrd=(ref($dx))?$dx->bpow(2):$dx**2;
			my $dy=(ref($pt_new->[1]))?$pt_new->[1]->copy()->bsub($pt_last->[1]):$pt_new->[1] - $pt_last->[1];
			my $dysqrd=(ref($dy))?$dy->bpow(2):$dy**2;
			my $distsqrd=(ref($dysqrd))?$dysqrd->copy()->badd($dxsqrd):$dysqrd + $dxsqrd;
			my $dist = (ref($distsqrd))?bigsqrt($distsqrd):sqrt($distsqrd);
			$ret = (ref($dim) ? $dim:Math::BigFloat->new(''.$dim)) - $dist;
			}
		return $ret;
		};

	my $newtheta;
	my $er;
	($newtheta,$er) = FalsePosition($findnexttheta,($direction ? [$theta,1]:[0,$theta]),$self->{resolution}/10,($direction ? ($theta + (1-$theta)/2):($theta/2)),'dimensionalStepFromTheta');
	#warn " dim step result ($newtheta,$er)\n";
	if (defined($er)) {
        warn "dimstep er: $er";
		#probably just reached the end
		if (abs(&{$findnexttheta}(($direction ? 1:0))) < $dim) {
			$newtheta=($direction ? 1:0);
			}
		#otherwise the error might be real
		}
	return ($self->point($newtheta),$newtheta);
	}
sub getInfiniteSlopeThetas {
	my $self=shift;
	my $bounds=shift;
	if (!defined($bounds)) {$bounds=[0,1];}
	my @infthetas;
	my ($seg,$segtheta,$segind);
	($seg,$segtheta,$segind)=$self->getSegThetaIndexAtPathTheta($bounds->[0]);
	for (my $i=$segind;$i<scalar(@{$self->{pathSegments}});$i++) {
		if ($self->{pathThetaToCompThetaRanges}->[$segind]->[1] > $bounds->[1]) {last;}
		push(@infthetas,$self->{pathSegments}->[$segind]->getInfiniteSlopeThetas());
		}
	@infthetas = grep {($_ > $bounds->[0] || $_ eq $bounds->[0]) && ($_ < $bounds->[1] || $_ eq $bounds->[1])} @infthetas;
	return @infthetas;
	}

sub getKeyPointsOnLine { ### Not quite right yet, but maybe usable
	my $self=shift;
	my @ret=();
	push(@ret,
		[$self->{pathSegments}->[0]->{maxx},$self->{pathSegments}->[0]->f($self->{pathSegments}->[0]->{maxx}),ref($self->{pathSegments}->[0])=~/bezier/i?$self->{pathSegments}->[0]->solveXforTheta($self->{pathSegments}->[0]->{maxx}):$self->{pathSegments}->[0]->{maxx}],
		[$self->{pathSegments}->[0]->{minx},$self->{pathSegments}->[0]->f($self->{pathSegments}->[0]->{minx}),ref($self->{pathSegments}->[0])=~/bezier/i?$self->{pathSegments}->[0]->solveXforTheta($self->{pathSegments}->[0]->{minx}):$self->{pathSegments}->[0]->{minx}],
		(map {[$_,$self->{pathSegments}->[0]->{maxy},ref($self->{pathSegments}->[0])=~/bezier/i?$self->{pathSegments}->[0]->solveYforTheta($_):$_] }   $self->{pathSegments}->[0]->F($self->{pathSegments}->[0]->{maxy})),
		(map {[$_,$self->{pathSegments}->[0]->{miny},ref($self->{pathSegments}->[0])=~/bezier/i?$self->{pathSegments}->[0]->solveYforTheta($_):$_] }   $self->{pathSegments}->[0]->F($self->{pathSegments}->[0]->{miny})),
		[$self->{pathSegments}->[0]->{p1}->[0],$self->{pathSegments}->[0]->{p1}->[1],ref($self->{pathSegments}->[0])=~/bezier/i?$self->{pathSegments}->[0]->solveXforTheta($self->{pathSegments}->[0]->{p1}->[0]):$self->{pathSegments}->[0]->{p1}->[0]]
		);
	if (ref($self->{pathSegments}->[0])=~/bezier/i) {print "sorting bezier points by thetas1\n";@ret = sort {$a->[2]<=>$b->[2]} @ret;}
	elsif (ref($self->{pathSegments}->[0])=~/line/i) {
		if ($self->{pathSegments}->[0]->[0]<$self->{pathSegments}->[0]->{p2}->[0]) {@ret = sort {$a->[0]<=>$b->[0]} @ret;}
		else {@ret = sort {$b->[0]<=>$a->[0]} @ret;}
		}
#	print "first sort numbers : " , join(",",map {$_->[2]} @ret),"\n";
	@ret = map {[$_->[0],$_->[1]]} @ret;

	push(@ret,[111,111]);#testing

	for (my $i=1;$i<scalar(@{$self->{pathSegments}});$i++) {
		# so far, assumes all segments have ->{p1} and ->{p2} start and end points
		my @thisgroup=(
			[$self->{pathSegments}->[$i]->{maxx},$self->{pathSegments}->[$i]->f($self->{pathSegments}->[$i]->{maxx}), ref($self->{pathSegments}->[$i])=~/bezier/i?$self->{pathSegments}->[$i]->solveXforTheta($self->{pathSegments}->[$i]->{maxx}):$self->{pathSegments}->[$i]->{maxx}],
			[$self->{pathSegments}->[$i]->{minx},$self->{pathSegments}->[$i]->f($self->{pathSegments}->[$i]->{minx}), ref($self->{pathSegments}->[$i])=~/bezier/i?$self->{pathSegments}->[$i]->solveXforTheta($self->{pathSegments}->[$i]->{minx}):$self->{pathSegments}->[$i]->{minx}],
			(map {[$_,$self->{pathSegments}->[$i]->{maxy}, ref($self->{pathSegments}->[$i])=~/bezier/i?$self->{pathSegments}->[$i]->solveYforTheta($_):$_]}   $self->{pathSegments}->[$i]->F($self->{pathSegments}->[$i]->{maxy})),
			(map {[$_,$self->{pathSegments}->[$i]->{miny}, ref($self->{pathSegments}->[$i])=~/bezier/i?$self->{pathSegments}->[$i]->solveYforTheta($_):$_]}   $self->{pathSegments}->[$i]->F($self->{pathSegments}->[$i]->{miny})),
			[$self->{pathSegments}->[$i-1]->{p2}->[0],$self->{pathSegments}->[$i-1]->{p2}->[1], ref($self->{pathSegments}->[$i-1])=~/bezier/i?$self->{pathSegments}->[$i-1]->solveXforTheta($self->{pathSegments}->[$i-1]->{p2}->[0]):$self->{pathSegments}->[$i-1]->{p2}->[0]],
			[$self->{pathSegments}->[$i]->{p1}->[0],  $self->{pathSegments}->[$i]->{p1}->[1],   ref($self->{pathSegments}->[$i])=~/bezier/i?$self->{pathSegments}->[$i]->solveXforTheta($self->{pathSegments}->[$i]->{p2}->[0]):$self->{pathSegments}->[$i]->{p2}->[0]]);
		my %dupsieve={};
		@thisgroup = grep {!$dupsieve{$_->[0].','.$_->[1]}++} @thisgroup;
		if (ref($self->{pathSegments}->[$i])=~/bezier/i) {
#			print "sorting bezier points by thetas2\n";
			@thisgroup = sort {$a->[2]<=>$b->[2]} @thisgroup;
#			print "in loop sort numbers $i: " , join(",",map {$_->[2]} @thisgroup),"\n";
			}
		elsif (ref($self->{pathSegments}->[$i])=~/line/i) {
			if ($self->{pathSegments}->[$i]->[0]<$self->{pathSegments}->[$i]->{p2}->[0]) {@thisgroup = sort {$a->[0]<=>$b->[0]} @thisgroup;}
			else {@thisgroup = sort {$b->[0]<=>$a->[0]} @thisgroup;}
			}
		push(@ret,map {[$_->[0],$_->[1]]} @thisgroup);
		push(@ret,[222,222]);#testing
		}
	push(@ret,$self->{pathSegments}->[scalar(@{$self->{pathSegments}})-1]->{p2});
	my %dupsieve={};
	@ret = grep {!$dupsieve{$_->[0].','.$_->[1]}++} @ret;
	return @ret;
	}


sub _rotate2d {
	my $origin=shift;
	my $point=shift;
	my $angle=shift;
	my $dx=($point->[0]-$origin->[0]);
	my $dy=($point->[1]-$origin->[1]);
	#$angle = ($angle - (2*$pi)*int($angle/(2*$pi)));#usually built into the trig functions
	#{a c-b d, a d+b c}
	return [$origin->[0] + ($dx*cos($angle) - $dy*sin($angle)),$origin->[1] + ($dx*sin($angle) + $dy*cos($angle))];
	}

}
####################################################################################
###      Math::MikePath::BezierCubicSegment   ######################################
package Math::MikePath::BezierCubicSegment;
{
use Math::CubicFormula;
use Math::QuadraticFormula;
use Math::Function::Root qw(BrentsMethod Bisection FalsePosition);
our $pi = 4 * atan2(1,1);
our $pi_big = Math::BigFloat->new('3.141592653589793238462643383279502884197169399375105820974944592308'); # for use possibly with new built-in cubic solver, unless we end up factoring it out
our $piovertwo = 2 * atan2(1,1);
our $piovertwo_big = $pi_big->copy()->bdiv(2); # for use possibly with new built-in cubic solver, unless we end up factoring it out
our $twopi = 8 * atan2(1,1);
our $twopi_big = $pi_big->copy()->bmul(2); # for use possibly with new built-in cubic solver, unless we end up factoring it out
our $fourpi = 16 * atan2(1,1);
our $fourpi_big = $pi_big->copy()->bmul(4); # for use possibly with new built-in cubic solver, unless we end up factoring it out

use Time::HiRes qw(gettimeofday tv_interval);

sub new {
	my $class = shift;
	my $self={};
	bless $self,$class;
	$self->{p1} = shift;
	$self->{cp1}= shift;
	$self->{cp2}= shift;
	$self->{p2} = shift;
	$self->{precision} = shift;
	$self->{isLite} = @_ ? shift:0;
	$self->{maxdiaglength} = (reverse sort {$a<=>$b} map {sqrt(($_->[3] - $_->[2])**2 + ($_->[1] - $_->[0])**2)} ([@{$self->{p1}},@{$self->{cp2}}],[@{$self->{cp1}},@{$self->{p2}}]))[0];
	#$self->{thetaprecision} = $self->{precision}/$self->{maxdiaglength};
	$self->{A}  = $self->{p2}->[0] - 3 * $self->{cp2}->[0] + 3 * $self->{cp1}->[0] -     $self->{p1}->[0];
	$self->{B}  =                    3 * $self->{cp2}->[0] - 6 * $self->{cp1}->[0] + 3 * $self->{p1}->[0];
	$self->{C}  =                                            3 * $self->{cp1}->[0] - 3 * $self->{p1}->[0];
	$self->{D}  =                                                                        $self->{p1}->[0];
	$self->{E}  = $self->{p2}->[1] - 3 * $self->{cp2}->[1] + 3 * $self->{cp1}->[1] -     $self->{p1}->[1];
	$self->{F}  =                    3 * $self->{cp2}->[1] - 6 * $self->{cp1}->[1] + 3 * $self->{p1}->[1];
	$self->{G}  =                                            3 * $self->{cp1}->[1] - 3 * $self->{p1}->[1];
	$self->{H}  =                                                                        $self->{p1}->[1];
	my $bA;
	if (abs($self->{A}) < 0.0001) {$bA=Math::BigFloat->new(''.$self->{A})} #just discovered this is a rare problem that pushes the limits of arctan at +/-1 in CubicFormula() stuff
	if ($self->{B} eq 0) {$self->{BdA}=0;} else {$self->{BdA}=(!defined($bA))?$self->{B}/$self->{A} : (Math::BigFloat->new(''.$self->{B}))->bdiv($bA) ;}
	if ($self->{C} eq 0) {$self->{CdA}=0;} else {$self->{CdA}=(!defined($bA))?$self->{C}/$self->{A} : (Math::BigFloat->new(''.$self->{C}))->bdiv($bA) ;}
	my $bE;
	if (abs($self->{E}) < 0.0001) {$bE=Math::BigFloat->new(''.$self->{E})} # same as for A above ....  you know, if E or A somehow came out to exactly zero... what kind of bezier does that?... does it degrade to quadratic bezier? or to a circle/arc formula? worth playing with at some point
	if ($self->{F} eq 0) {$self->{FdE}=0;} else {$self->{FdE}=(!defined($bE))?$self->{F}/$self->{E} : (Math::BigFloat->new(''.$self->{F}))->bdiv($bE) ;}
	if ($self->{G} eq 0) {$self->{GdE}=0;} else {$self->{GdE}=(!defined($bE))?$self->{G}/$self->{E} : (Math::BigFloat->new(''.$self->{G}))->bdiv($bE) ;}

	$self->{Am3}=$self->{A} * 3;
	$self->{Bm2}=$self->{B} * 2;
	$self->{Em3}=$self->{E} * 3;
	$self->{Fm2}=$self->{F} * 2;
	$self->{Am6}=$self->{A} * 6;
	$self->{Em6}=$self->{E} * 6;

	if (!$self->{isLite}) {
		$self->initBigs();
		}

	my @extremexs_is=(0,((!$self->{isLite})?$self->solveXPrimeforThetaBig(Math::BigFloat->bzero()):$self->solveXPrimeforTheta(0)),1);
	my @extremexs = map {(ref($_) && !$self->{isLite}) ? $self->bezierEvalXofTBig($_):$self->bezierEvalXofT($_)} @extremexs_is;
	my @extremexs_sorted = sort {$a<=>$b} @extremexs;
	my @extremeys_is=(0,((!$self->{isLite})?$self->solveYPrimeforThetaBig(Math::BigFloat->bzero()):$self->solveYPrimeforTheta(0)),1);
	my @extremeys = map {(ref($_) && !$self->{isLite}) ? $self->bezierEvalYofTBig($_):$self->bezierEvalYofT($_)} @extremeys_is;
	my @extremeys_sorted = sort {$a<=>$b} @extremeys;

	$self->{extremexs_is}=[(@extremexs_is)];
	$self->{extremeys_is}=[(@extremeys_is)];
	$self->{extremexs}=[(@extremexs)];
	$self->{extremeys}=[(@extremeys)];
	$self->{minx} = $extremexs_sorted[0];
	$self->{maxx} = $extremexs_sorted[$#extremexs];
	$self->{miny} = $extremeys_sorted[0];
	$self->{maxy} = $extremeys_sorted[$#extremeys];

	#FOLLOWING (and above and other repeated in initDangerRanges function)
	#profiled slice making task with DProfLB and found that
	#20% to 60% of time is taking doing inRange checks for beziers
	#probably because those checks are using these min/max x/y vals
	#that are BigFloats. So let's make regular PERL scalar number versions
	#and use those in the inRange tests. Will probably reintroduce
	#some boundary problems, but it might also give a 50%+ speedup.

#	$self->{minx_notbig} = ref($self->{minx}) ? eval(sprintf("%.20f",$self->{minx}->bstr())):$self->{minx};
#	$self->{maxx_notbig} = ref($self->{maxx}) ? eval(sprintf("%.20f",$self->{maxx}->bstr())):$self->{maxx};
#	$self->{miny_notbig} = ref($self->{miny}) ? eval(sprintf("%.20f",$self->{miny}->bstr())):$self->{miny};
#	$self->{maxy_notbig} = ref($self->{maxy}) ? eval(sprintf("%.20f",$self->{maxy}->bstr())):$self->{maxy};

# SPEED ATTEMPTS <-- USE THIS COMMENT ON THESE - trying to do little things to speed up MikePath instanciation
# use that comment so you can go back and find all the places where these speed up attempts end up breking everything
# eval is dumb. just go with the sprintf if you can
	$self->{minx_notbig} = ref($self->{minx}) ? 0 + sprintf("%.20f",$self->{minx}->bstr()):$self->{minx};
	$self->{maxx_notbig} = ref($self->{maxx}) ? 0 + sprintf("%.20f",$self->{maxx}->bstr()):$self->{maxx};
	$self->{miny_notbig} = ref($self->{miny}) ? 0 + sprintf("%.20f",$self->{miny}->bstr()):$self->{miny};
	$self->{maxy_notbig} = ref($self->{maxy}) ? 0 + sprintf("%.20f",$self->{maxy}->bstr()):$self->{maxy};

	#print "minx_notbig: $self->{minx_notbig}\n";
	#print "maxx_notbig: $self->{maxx_notbig}\n";
 	#print "miny_notbig: $self->{miny_notbig}\n";
	#print "maxy_notbig: $self->{maxy_notbig}\n";

# There are two cases you need to guard against
#1. f'(x) or F'(y) are too small to produce unique answers using specified and available precision
#2. m_ix or m_iy are too small to give unique answers for current-precision-sized increments of x or y
#I think the best way to ensure good results while maintaining most of the speed that results from ignoring this issue
#is to determine ahead of time what level of precision is needed over different intervals of the xy curve. And then
#ensure that that level of precision is in effect when each point lookup is made.
#There will be a generally safe intervals where the required precision will usually be present. Here you will simply check for presense of required precision, and then upgrade if necessary.
#In the more dangerous intervals, we will immediately go into calculating how much extra precision is needed.
#Default for PERL appears to be to use DOUBLEs, whose constant accuracy you can roughly translate into current precision
#for a given number by subtracting the number of digits left of decimal from total avail (15 I think).
#Another thing to consider in making safe and unsafe intervals is, then, magnitude of x or y on the left side of decimal.
#Interval[0,99] has 14 digit precision. Interval [100,999] has 13.
#It would be nice to be able to pass PERL native floats into calculations, but still be able to demand calculation to certain precision, which may exceed PERL float native precision.
#This would allow delay of upgrade to some higher precision number format. If the calculation subroutine implements its own method of doing exact arithmatic with the PERL native floats, those passed-in floats could stay PERL native (and fast, right?).
#Only return vals would have to be made arb. precision.


#print "### EXTREMES ###\n";
#print "ext xs at coords:",join("\n",map {'theta: '.$_.' x: '.$self->bezierEvalXofT($_).' y: '.$self->bezierEvalYofT($_)} $self->solveXPrimeforTheta(0.01)),"\n";
#print "ext xs at coords:",join("\n",map {'theta: '.$_.' x: '.$self->bezierEvalXofT($_).' y: '.$self->bezierEvalYofT($_)} $self->solveXPrimeforTheta(0.0)),"\n";
#print "ext xs at coords:",join("\n",map {'theta: '.$_.' x: '.$self->bezierEvalXofT($_).' y: '.$self->bezierEvalYofT($_)} $self->solveXPrimeforTheta(-0.01)),"\n";
#print "ext ys at coords:",join("\n",map {'theta: '.$_.' x: '.$self->bezierEvalXofT($_).' y: '.$self->bezierEvalYofT($_)} $self->solveYPrimeforTheta(0.01)),"\n";
#print "ext ys at coords:",join("\n",map {'theta: '.$_.' x: '.$self->bezierEvalXofT($_).' y: '.$self->bezierEvalYofT($_)} $self->solveYPrimeforTheta(0.0)),"\n";
#print "ext ys at coords:",join("\n",map {'theta: '.$_.' x: '.$self->bezierEvalXofT($_).' y: '.$self->bezierEvalYofT($_)} $self->solveYPrimeforTheta(-0.01)),"\n";

	if (!$self->{isLite}) {
		$self->initDangerRanges();
		}
	if (!$self->{isLite}) {
	#	$self->{length}=getLength($self,1000);
		}




    # maybe new stuff for built-in cubic solver
    # these shouldn't be recomputed every time the solver runs
    # do it once here

    $self->{cubA} = $self->{BdA};
    $self->{cubB} = $self->{CdA};
    $self->{DdA} = $self->{D}/$self->{A};
    $self->{cubQ} = (3*$self->{cubB} - $self->{cubA}**2)/9.0;

    # don't use, except for testing/explore - too slow to call as anon sub
    $self->{cubCofX} = sub { $self->{DdA} - $_[0]/$self->{A} };
    $self->{cubRofX} = sub { (9.0*$self->{cubA}*$self->{cubB} - 27.0*$self->{cubCofX}->($_[0]) - 2.0*$self->{cubA}**3)/54.0 };
    $self->{cubDofX} = sub { $self->{cubQ}**3 + $self->{cubRofX}->($_[0])**2 };
    $self->{cubicFormulaAlt} = sub {
        my $x = $_[0];
        my $R = $self->{cubRofX}->($x);
        my $D = $self->{cubQ}**3 + $R**2;              # polynomial discriminant
        my $Dpos = $D > 0 || $D eq 0;
        my $sqrtD = sqrt($D) if $Dpos;
        my $preS=$R + $sqrtD if $Dpos;
        my $preT=$R - $sqrtD if $Dpos;
        my $SpT= (($preS < 0)?-1:1) * (abs($preS)**(1/3))
               + (($preT < 0)?-1:1) * (abs($preT)**(1/3)) if $Dpos;

        $Dpos
        ?

        [
        -$self->{cubAd3} + $SpT
        ,
        (($D eq 0)
         ?(-$self->{cubAd3} - $SpT/2,-$self->{cubAd3} - $SpT/2)   #dup real root
         :()
        )
        ]

        :

        [
        #(1/3)
        #*(2*($R/$self->{cubQ}) - $self->{cubA})

        2*sqrt(-$self->{cubQ}) * (cos(  acos($R/sqrt(-$self->{cubQ}**3)           )/3)) - $self->{cubAd3}

        ,
        #(1/(3*$self->{cubQ}))
        #* ( -$R - sqrt( -(1/3)*$D ) )
        #- $self->{cubA}/3
        2*sqrt(-$self->{cubQ}) * (cos( (acos($R/sqrt(-$self->{cubQ}**3)) + 2*$pi  )/3)) - $self->{cubAd3}

        ,
        #(1/(3*$self->{cubQ}))
        #* ( -$R + sqrt( -(1/3)*$D ) )
        #- $self->{cubA}/3
        2*sqrt(-$self->{cubQ}) * (cos( (acos($R/sqrt(-$self->{cubQ}**3)) + 4*$pi  )/3)) - $self->{cubAd3}

        ]
    };
    $self->{cubicFormulaAlt} = undef;

    $self->{cubAd3} = $self->{cubA}/3;

    # lots/all this stuff is only for if $self->{cubQ} < 0

    if ($self->{cubQ} < 0) {
        $self->{cubsqrtofnegQ} = sqrt(-1 * $self->{cubQ});
        $self->{cubtwotimessqrtofnegQ} = 2*sqrt(-1 * $self->{cubQ});
        $self->{cubsqrtofnegQcubed} = sqrt( -1.0 * ($self->{cubQ}**3) );


        $self->{cubtheta_slope_upper_bound} = (1 + $self->{cubAd3})/$self->{cubtwotimessqrtofnegQ} ;
        $self->{cubtheta_slope_lower_bound} = (    $self->{cubAd3})/$self->{cubtwotimessqrtofnegQ} ;

        # THIS IS PROBABLY BAD
        # YOU MIGHT HAVE THE OPPORTUNITY TO RULE OUT ONE OR MORE OF THE CUBIC SOLVER EXPRESSIONS IF THESE ARE OUT OF RANGE??? INSTEAD OF SNAPPING TO OK RANGE
        # THAT COULD BE A NICE SPEEDUP. COULD CONFIGURE THE SOLVER FUNCTION SO IT DOESN'T EVEN HAVE THOSE EXPRESSIONS - MAYBE MAKE THE SOLVER AS ANON SUB HERE
        $self->{cubtheta_slope_upper_bound} =  1 if $self->{cubtheta_slope_upper_bound} >  1 ;
        $self->{cubtheta_slope_lower_bound} = -1 if $self->{cubtheta_slope_lower_bound} < -1 ;

        my $acos_upper = acos($self->{cubtheta_slope_upper_bound});
        my $acos_lower = acos($self->{cubtheta_slope_lower_bound});


        $acos_upper2 = $acos_upper;
        $acos_lower2 = $acos_lower;
        $acos_upper3 = $acos_upper;
        $acos_lower3 = $acos_lower;



        if ($acos_upper<$acos_lower) {my $swap=$acos_upper;$acos_upper=$acos_lower;$acos_lower=$swap;}
        if ($acos_upper2<$acos_lower2) {my $swap=$acos_upper2;$acos_upper2=$acos_lower2;$acos_lower2=$swap;}
        if ($acos_upper3<$acos_lower3) {my $swap=$acos_upper3;$acos_upper3=$acos_lower3;$acos_lower3=$swap;}


        # THESE ARE IMPORTANT, BUT IM GUESSING AT THEM - CANT QUITE GET HOW/WHY THEY WORK

        # guessing... maybe - worked for one case
    # maybe leave this one alone
        #$acos_upper -= $pi ;#if ($acos_upper*3 > $pi);
        #$acos_lower -= $pi ;#if ($acos_lower*3 > $pi);
        # guessing... maybe - worked for one case
        $acos_upper2 -= 2*$pi/3 ;#if ($acos_upper2*3 > $pi);
        $acos_lower2 -= 2*$pi/3 ;#if ($acos_lower2*3 > $pi);
        # guessing...
        # this one seems right
        $acos_upper3 -= $pi/3 ;#if ($acos_upper3*3 > $pi);
        $acos_lower3 -= $pi/3 ;#if ($acos_lower3*3 > $pi);


        $self->{cubtheta_upper_bound1} = $acos_upper*3 ;
        $self->{cubtheta_lower_bound1} = $acos_lower*3 ;
        #$self->{cubtheta_upper_bound1} = $twopi if ($self->{cubtheta_upper_bound1} > $twopi);
        #$self->{cubtheta_lower_bound1} = $twopi if ($self->{cubtheta_lower_bound1} > $twopi);

        $self->{cubtheta_upper_bound2} = ($acos_upper2*3 + $twopi) ;
        $self->{cubtheta_lower_bound2} = ($acos_lower2*3 + $twopi) ;
        #$self->{cubtheta_upper_bound2} = $fourpi if ($self->{cubtheta_upper_bound2} > $fourpi);
        #$self->{cubtheta_lower_bound2} = $fourpi if ($self->{cubtheta_lower_bound2} > $fourpi);

        $self->{cubtheta_upper_bound3} = ($acos_upper3*3 + $fourpi) ;
        $self->{cubtheta_lower_bound3} = ($acos_lower3*3 + $fourpi) ;
        #$self->{cubtheta_upper_bound3} = $fourpi + $twopi if ($self->{cubtheta_upper_bound2} > $fourpi + $twopi);
        #$self->{cubtheta_lower_bound3} = $fourpi + $twopi if ($self->{cubtheta_lower_bound2} > $fourpi + $twopi);

=cut

        # Those angle bounds might be right now - different bounds for each of the
        # three expressions for roots in the D<0 part of the cubic solver.
        # Can we now map those bounds for each case back to ... bounds for the
        # solver's C input, and that back to x ranges corresponding to which
        # expressions give results for those x ranges? and the t to x mappings for those?

        # like, maybe, for each bound
        # since cos(bound) == R/sqrtofnegQcubed  (eh??)
        # R = cos(bound)*sqrtofnegQcubed
        # (9AB-27C-2A^3)/54 = cos(bound)*sqrtofnegQcubed
        # -27C = cos(bound)*sqrtofnegQcubed*54 + 2A^3 - 9AB
        # C = (9AB - 2A^3 - cos(bound)*sqrtofnegQcubed*54) / 27
        # ($self->{D}-x)/$self->{A} =  (9AB - 2A^3 - cos(bound)*sqrtofnegQcubed*54) / 27
        # -x =  (($self->{A} * (9AB - 2A^3 - cos(bound)*sqrtofnegQcubed*54)) / 27) - $self->{D}
        # x =  $self->{D} - (($self->{A} * (9AB - 2A^3 - cos(bound)*sqrtofnegQcubed*54)) / 27)
        my $boundx_lower_1 = $self->{D} - (($self->{A} * (9*$self->{cubA}*$self->{cubB} - 2*$self->{cubA}**3 - cos($self->{cubtheta_lower_bound1})*$self->{cubsqrtofnegQcubed}*54)) / 27);
        my $boundx_upper_1 = $self->{D} - (($self->{A} * (9*$self->{cubA}*$self->{cubB} - 2*$self->{cubA}**3 - cos($self->{cubtheta_upper_bound1})*$self->{cubsqrtofnegQcubed}*54)) / 27);
        my $boundx_lower_2 = $self->{D} - (($self->{A} * (9*$self->{cubA}*$self->{cubB} - 2*$self->{cubA}**3 - cos($self->{cubtheta_lower_bound2})*$self->{cubsqrtofnegQcubed}*54)) / 27);
        my $boundx_upper_2 = $self->{D} - (($self->{A} * (9*$self->{cubA}*$self->{cubB} - 2*$self->{cubA}**3 - cos($self->{cubtheta_upper_bound2})*$self->{cubsqrtofnegQcubed}*54)) / 27);
        my $boundx_lower_3 = $self->{D} - (($self->{A} * (9*$self->{cubA}*$self->{cubB} - 2*$self->{cubA}**3 - cos($self->{cubtheta_lower_bound3})*$self->{cubsqrtofnegQcubed}*54)) / 27);
        my $boundx_upper_3 = $self->{D} - (($self->{A} * (9*$self->{cubA}*$self->{cubB} - 2*$self->{cubA}**3 - cos($self->{cubtheta_upper_bound3})*$self->{cubsqrtofnegQcubed}*54)) / 27);
        warn "boundx_lower_1: $boundx_lower_1  [",((180/$pi)*$self->{cubtheta_lower_bound1}/3),"]\n";
        warn "boundx_upper_1: $boundx_upper_1  [",((180/$pi)*$self->{cubtheta_upper_bound1}/3),"]\n";
        warn "boundx_lower_2: $boundx_lower_2  [",((180/$pi)*$self->{cubtheta_lower_bound2}/3),"]\n";
        warn "boundx_upper_2: $boundx_upper_2  [",((180/$pi)*$self->{cubtheta_upper_bound2}/3),"]\n";
        warn "boundx_lower_3: $boundx_lower_3  [",((180/$pi)*$self->{cubtheta_lower_bound3}/3),"]\n";
        warn "boundx_upper_3: $boundx_upper_3  [",((180/$pi)*$self->{cubtheta_upper_bound3}/3),"]\n";


        # should you include this in that solving???
        # my $root = $self->{cubtwotimessqrtofnegQ} * cos($tocos2/3) - $self->{cubAd3};
        # where tocos is where you'd put your bound, and then solve for...
        # cos( tocos / 3 ) = (root + $self->{cubAd3}) / $self->{cubtwotimessqrtofnegQ}
        # tocos = asin( (root + $self->{cubAd3}) / $self->{cubtwotimessqrtofnegQ}) * 3

        # for the ones that seem to have valid bounds but the expression evals don't ever get in there...
        # maybe do a test eval in the center of each bound range and if it's not within 0 to 1, don't use that bound or expression
        # simplist to disable expression right now is just make sure the first conditional guarding it will always fail


        # YOU ARE HERE - arround here, noodling. Come back fresh later.
        # go do more directly useful things for a bit, then come back w clearer idea of what doing here.


        # those are about right
        # though for case where only one expression always has a root in 0 to 1 range
        # the x bounds for all the expressions as figured above is the the correct x range
        # for just that last expression. the other two expressions shouldn't and dont eval down in solver.
        #
        # something's lost in the cosine... how to account for that?



        warn "cubic theta slope bounds:\n$self->{cubtheta_slope_upper_bound}\n$self->{cubtheta_slope_lower_bound}\n";
        warn "cubic theta bounds:\n";
        warn "$self->{cubtheta_upper_bound1} > $self->{cubtheta_lower_bound1}\n";
        warn "$self->{cubtheta_upper_bound2} > $self->{cubtheta_lower_bound2}\n";
        warn "$self->{cubtheta_upper_bound3} > $self->{cubtheta_lower_bound3}\n";

=cut

        }

	return $self;


	}

# lazy creation of some BigFloats
sub cubtwotimessqrtofnegQ_big {
    return $self->{cubtwotimessqrtofnegQ_big} if defined($self->{cubtwotimessqrtofnegQ_big});
    $self->{cubtwotimessqrtofnegQ_big} = ($self->cubQ_big())->copy()->bmul(-1.0);
    $self->{cubtwotimessqrtofnegQ_big} = bigsqrt($self->{cubtwotimessqrtofnegQ_big});
    $self->{cubtwotimessqrtofnegQ_big}->bmul(2.0);
    return $self->{cubtwotimessqrtofnegQ_big};
    }
sub cubQ_big {
    return $self->{cubQ_big} if defined($self->{cubQ_big});
    $self->{cubQ_big} = Math::BigFloat->new($self->{cubQ});
    return $self->{cubQ_big};
    }
sub cubsqrtofnegQcubed_big {
    return $self->{cubsqrtofnegQcubed_big} if defined($self->{cubsqrtofnegQcubed_big});
    my $presqrtofnegQcubed = ($self->cubQ_big())->copy()->bpow(3)->bmul(-1.0);
    $self->{cubsqrtofnegQcubed_big} = bigsqrt($presqrtofnegQcubed);
    return $self->{cubsqrtofnegQcubed_big};
    }

sub initDangerRanges {
	my $self=shift;


#redo these in all big - this might muck stuff up, so might delete this section later
	my @extremexs_is=(0,($self->solveXPrimeforThetaBig(Math::BigFloat->bzero())),1);
	#my @extremexs_is=(0,($self->solveXPrimeforThetaBig(Math::BigFloat->bzero())),Math::BigFloat->bone());
	my @extremexs = map {ref($_)?$self->bezierEvalXofTBig($_):$self->bezierEvalXofT($_)} @extremexs_is;
	my @extremexs_sorted = sort {$a<=>$b} @extremexs;

	#my @extremeys_is=(0,($self->solveYPrimeforThetaBig(Math::BigFloat->bzero())),1);
	my @extremeys_is=(Math::BigFloat->bzero(),($self->solveYPrimeforThetaBig(Math::BigFloat->bzero())),Math::BigFloat->bone());
	#my @extremeys_is=(0,($self->solveYPrimeforThetaBig(Math::BigFloat->bzero())),Math::BigFloat->bone());
	my @extremeys = map {ref($_)?$self->bezierEvalYofTBig($_):$self->bezierEvalYofT($_)} @extremeys_is;
	my @extremeys_sorted = sort {$a<=>$b} @extremeys;

	#print "extremeys:\n  ",join("\n  ",@extremeys),"\n";

	$self->{extremexs_is}=[(@extremexs_is)];
	$self->{extremeys_is}=[(@extremeys_is)];
	$self->{extremexs}=[(@extremexs)];
	$self->{extremeys}=[(@extremeys)];

    # Making these for getFeet(), since that gets called so much and shouldn't
    # have to do the expensive bstr() every time.
    my %eisnb_dups;
    $self->{extreme_is_sorted_notbig} = [(sort {$a<=>$b} grep {!$eisnb_dups{$_}++} ((map {ref($_)?$_->bstr():$_} @{$self->{extremexs_is}}),(map {ref($_)?$_->bstr():$_} @{$self->{extremeys_is}})))];
    $self->{tangents_at_sorted_extremes} = [(map {ref($_)?$_->bstr():$_} map {$self->slopeTangent_byTheta($_)} ( @{$self->{extreme_is_sorted_notbig}} ))];

	$self->{minx} = $extremexs_sorted[0];
	$self->{maxx} = $extremexs_sorted[$#extremexs];
	$self->{miny} = $extremeys_sorted[0];
	$self->{maxy} = $extremeys_sorted[$#extremeys];

	#profiled slice making task with DProfLB and found that
	#20% to 60% of time is taking doing inRange checks for beziers
	#probably because those checks are using these min/max x/y vals
	#that are BigFloats. So let's make regular PERL scalar number versions
	#and use those in the inRange tests. Will probably reintroduce
	#some boundary problems, but it might also give a 50%+ speedup.

# SPEED ATTEMPTS
#	$self->{minx_notbig} = ref($self->{minx}) ? eval(sprintf("%.20f",$self->{minx}->bstr())):$self->{minx};
#	$self->{maxx_notbig} = ref($self->{maxx}) ? eval(sprintf("%.20f",$self->{maxx}->bstr())):$self->{maxx};
#	$self->{miny_notbig} = ref($self->{miny}) ? eval(sprintf("%.20f",$self->{miny}->bstr())):$self->{miny};
#	$self->{maxy_notbig} = ref($self->{maxy}) ? eval(sprintf("%.20f",$self->{maxy}->bstr())):$self->{maxy};
	$self->{minx_notbig} = ref($self->{minx}) ? 0 + sprintf("%.20f",$self->{minx}->bstr()):$self->{minx};
	$self->{maxx_notbig} = ref($self->{maxx}) ? 0 + sprintf("%.20f",$self->{maxx}->bstr()):$self->{maxx};
	$self->{miny_notbig} = ref($self->{miny}) ? 0 + sprintf("%.20f",$self->{miny}->bstr()):$self->{miny};
	$self->{maxy_notbig} = ref($self->{maxy}) ? 0 + sprintf("%.20f",$self->{maxy}->bstr()):$self->{maxy};

	#print "minx_notbig: $self->{minx_notbig}\n";
	#print "maxx_notbig: $self->{maxx_notbig}\n";
 	#print "miny_notbig: $self->{miny_notbig}\n";
	#print "maxy_notbig: $self->{maxy_notbig}\n";
#end redo

	#my $perlprecision = Math::BigFloat->new('0.00000000000001');
	my $perlprecision =                       0.00000000000001;
	my $maxdim=(sort {$b<=>$a} map {abs($_)} ($self->{maxx},$self->{maxy},$self->{minx},$self->{miny}))[0];
	$maxdim=~/^([0-9]+)\./;
	#$self->{curveprecision}=$perlprecision * Math::BigFloat->new(''.(10**(length($1)))) ;
	$self->{curveprecision}=$perlprecision * (10**(length($1))) ;
	#print "curve precision: ",$self->{curveprecision},"\n";
	#was doing ...PrimeforThetaBig() for all these, but that takes about 1.5 seconds rather than something like 0.0003

#print "back to precision stuff: infinite slope range is greater than : self->{curveprecision}/perlprecision = $self->{curveprecision}/$perlprecision = ",($self->{curveprecision}/$perlprecision),"\n";
#print "in getting xdangeris:\n";
#print "		",join(",",$self->solveXPrimeforTheta($perlprecision/$self->{curveprecision}))  ," # near zero slope, positive\n";
#print "		",join(",",$self->solveXPrimeforTheta(0)),"                  # zero slope\n";
#print "		",join(",",$self->solveXPrimeforTheta(-$perlprecision/$self->{curveprecision})) ," # near zero slope, negative\n";
#print "		",join(",",$self->solveXPrimeforTheta($self->{curveprecision}/$perlprecision)) ,"  # toward infinite slope, positive\n";
#print "		",join(",",$self->solveXPrimeforTheta(-$self->{curveprecision}/$perlprecision)) ," # toward infinite slope, negative\n";

	my @xdangeris = sort {$a<=>$b} (
		($self->solveXPrimeforTheta($perlprecision/$self->{curveprecision}))  , # near zero slope, positive
	   #($self->solveXPrimeforTheta(Math::BigFloat->bzero())),                  # zero slope
		($self->solveXPrimeforTheta(0)),                  # zero slope
		($self->solveXPrimeforTheta(-$perlprecision/$self->{curveprecision})) , # near zero slope, negative
		($self->solveXPrimeforTheta($self->{curveprecision}/$perlprecision)) ,  # toward infinite slope, positive
		($self->solveXPrimeforTheta(-$self->{curveprecision}/$perlprecision)) , # toward infinite slope, negative
		);
	my @ydangeris = sort {$a<=>$b} (
		($self->solveYPrimeforTheta($perlprecision/$self->{curveprecision}))  ,
	   #($self->solveYPrimeforTheta(Math::BigFloat->bzero())) ,
		($self->solveYPrimeforTheta(0)) ,
		($self->solveYPrimeforTheta(-$perlprecision/$self->{curveprecision})) ,
		($self->solveYPrimeforTheta($self->{curveprecision}/$perlprecision)) ,
		($self->solveYPrimeforTheta(-$self->{curveprecision}/$perlprecision)) ,
		);

	my @mxidangerranges;
	for (my $i=0;$i<@xdangeris;$i++) {
		my $p=$self->bezierEvalXPrimeofT($xdangeris[$i]);
		my $pp=$self->bezierEvalXDoublePrimeofT($xdangeris[$i]);
		if (($p < 0 && $pp < 0) || (($p > 0 || $p eq 0) && ($pp > 0 || $pp eq 0))) {
			#is end of range
			if ($i eq 0)          {push(@mxidangerranges,[0,(sort {$a<=>$b} (1,($xdangeris[$i] + $self->{curveprecision})))[0]]);}
			else                  {push(@mxidangerranges,[(sort {$b<=>$a} (0,($xdangeris[$i-1] - $self->{curveprecision})))[0],(sort {$a<=>$b} (1,($xdangeris[$i] + $self->{curveprecision})))[0]]);}
			}
		elsif ($i eq $#xdangeris) {push(@mxidangerranges,[(sort {$b<=>$a} (0,($xdangeris[$i] - $self->{curveprecision})))[0],1]);}
		}
	$self->{mxidangerranges} = \@mxidangerranges;

	$self->{xofidangerranges} = [];
	push @{$self->{xofidangerranges}}, [$self->{p1}->[0],$self->{p1}->[0]]; #zero-length "range" to
	push @{$self->{xofidangerranges}}, [$self->{p2}->[0],$self->{p2}->[0]]; #try to be more exact at endpoints

	foreach (@mxidangerranges) {
		#$self->{xofidangerranges}->[scalar(@{$self->{xofidangerranges}})]=[eval($self->bezierEvalXofTBig($_->[0])->bstr()),eval($self->bezierEvalXofTBig($_->[1])->bstr())];
		push @{$self->{xofidangerranges}}, [$self->bezierEvalXofT($_->[0]),$self->bezierEvalXofT($_->[1])];
		}
	#print "danger xi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @mxidangerranges),' from xis: (',join(',',@xdangeris),')' ,"\n";
	#print "danger xofi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @{$self->{xofidangerranges}}) , "\n";

	my @myidangerranges;
	$self->{yofidangerranges} = [];
	$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[$self->{p1}->[1],$self->{p1}->[1]];
	$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[$self->{p2}->[1],$self->{p2}->[1]]; #try to be more exact at endpoints
	for (my $i=0;$i<@ydangeris;$i++) {
		my $p=$self->bezierEvalYPrimeofT($ydangeris[$i]);
		my $pp=$self->bezierEvalYDoublePrimeofT($ydangeris[$i]);
		if (($p < 0 && $pp < 0) || (($p > 0 || $p eq 0) && ($pp > 0 || $pp eq 0))) {
			#is end of range
			if ($i == 0)          {push(@myidangerranges,[0,(sort {$a<=>$b} (1,$ydangeris[$i] + $self->{curveprecision}))[0]]);}
			else                  {push(@myidangerranges,[(sort {$b<=>$a} (0,$ydangeris[$i-1] - $self->{curveprecision}))[0],(sort {$a<=>$b} (1,$ydangeris[$i] + $self->{curveprecision}))[0]]);}
			}
		elsif ($i == $#ydangeris) {push(@myidangerranges,[(sort {$b<=>$a} (0,$ydangeris[$i] - $self->{curveprecision}))[0],1]);}
		}
	$self->{myidangerranges} = \@myidangerranges;
	foreach (@myidangerranges) {
		#$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[eval($self->bezierEvalYofTBig($_->[0])->bstr()),eval($self->bezierEvalYofTBig($_->[1])->bstr())];
        # for f(x) you're using non-Big versions in this case. Want to do that here?
		$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[$self->bezierEvalYofTBig($_->[0]),$self->bezierEvalYofTBig($_->[1])];
		}
	#print "danger yi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @myidangerranges),' from yis: (',join(',',@ydangeris),')' ,"\n";
	#print "danger yofi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @{$self->{yofidangerranges}}) , "\n";
	}
sub initBigs_old {
	my $self=shift;
	$self->{A_Big}  = new Math::BigFloat '' . ($self->{p2}->[0] - 3 * $self->{cp2}->[0] + 3 * $self->{cp1}->[0] -     $self->{p1}->[0]);
	$self->{B_Big}  = new Math::BigFloat '' . (                   3 * $self->{cp2}->[0] - 6 * $self->{cp1}->[0] + 3 * $self->{p1}->[0]);
	$self->{C_Big}  = new Math::BigFloat '' . (                                           3 * $self->{cp1}->[0] - 3 * $self->{p1}->[0]);
	$self->{D_Big}  = new Math::BigFloat '' . (                                                                       $self->{p1}->[0]);
	$self->{E_Big}  = new Math::BigFloat '' . ($self->{p2}->[1] - 3 * $self->{cp2}->[1] + 3 * $self->{cp1}->[1] -     $self->{p1}->[1]);
	$self->{F_Big}  = new Math::BigFloat '' . (                   3 * $self->{cp2}->[1] - 6 * $self->{cp1}->[1] + 3 * $self->{p1}->[1]);
	$self->{G_Big}  = new Math::BigFloat '' . (                                           3 * $self->{cp1}->[1] - 3 * $self->{p1}->[1]);
	$self->{H_Big}  = new Math::BigFloat '' . (                                                                       $self->{p1}->[1]);
	if (eval($self->{B_Big}->bstr) eq 0) {$self->{BdA_Big}=Math::BigFloat->bzero;} else {$self->{BdA_Big}=new Math::BigFloat '' . ($self->{B_Big}/$self->{A_Big});}
	if (eval($self->{C_Big}->bstr) eq 0) {$self->{CdA_Big}=Math::BigFloat->bzero;} else {$self->{CdA_Big}=new Math::BigFloat '' . ($self->{C_Big}/$self->{A_Big});}
	if (eval($self->{F_Big}->bstr) eq 0) {$self->{FdE_Big}=Math::BigFloat->bzero;} else {$self->{FdE_Big}=new Math::BigFloat '' . ($self->{F_Big}/$self->{E_Big});}
	if (eval($self->{G_Big}->bstr) eq 0) {$self->{GdE_Big}=Math::BigFloat->bzero;} else {$self->{GdE_Big}=new Math::BigFloat '' . ($self->{G_Big}/$self->{E_Big});}
	$self->{Am3_Big}=$self->{A_Big} * 3;
	$self->{Bm2_Big}=$self->{B_Big} * 2;
	$self->{Em3_Big}=$self->{E_Big} * 3;
	$self->{Fm2_Big}=$self->{F_Big} * 2;
	$self->{Am6_Big}=$self->{A_Big} * 6;
	$self->{Em6_Big}=$self->{E_Big} * 6;
	$self->{bigInitted}=1;
	}
sub initBigs {
	my $self=shift;
	my $bp1x =new Math::BigFloat '' .$self->{p1}->[0];
	my $bp1y =new Math::BigFloat '' .$self->{p1}->[1];
	my $bcp1x=new Math::BigFloat '' .$self->{cp1}->[0];
	my $bcp1y=new Math::BigFloat '' .$self->{cp1}->[1];
	my $bcp2x=new Math::BigFloat '' .$self->{cp2}->[0];
	my $bcp2y=new Math::BigFloat '' .$self->{cp2}->[1];
	my $bp2x =new Math::BigFloat '' .$self->{p2}->[0];
	my $bp2y =new Math::BigFloat '' .$self->{p2}->[1];
	$self->{A_Big}  =  ($bp2x - $bcp2x * 3 + $bcp1x * 3 -     $bp1x);
	$self->{B_Big}  =  (                   $bcp2x * 3 - $bcp1x * 6 + $bp1x * 3);
	$self->{C_Big}  =  (                                           $bcp1x * 3 - $bp1x * 3);
	$self->{D_Big}  =  (                                                                       $bp1x);
	$self->{E_Big}  =  ($bp2y - $bcp2y * 3 + $bcp1y * 3 -     $bp1y);
	$self->{F_Big}  =  (                   $bcp2y * 3 - $bcp1y * 6 + $bp1y * 3);
	$self->{G_Big}  =  (                                           $bcp1y * 3 - $bp1y * 3);
	$self->{H_Big}  =  (                                                                       $bp1y);
# SPEED ATTEMPTS
#	if (eval($self->{B_Big}->bstr) eq 0) {$self->{BdA_Big}=Math::BigFloat->bzero;} else {$self->{BdA_Big}=($self->{B_Big}/$self->{A_Big});}
#	if (eval($self->{C_Big}->bstr) eq 0) {$self->{CdA_Big}=Math::BigFloat->bzero;} else {$self->{CdA_Big}=($self->{C_Big}/$self->{A_Big});}
#	if (eval($self->{F_Big}->bstr) eq 0) {$self->{FdE_Big}=Math::BigFloat->bzero;} else {$self->{FdE_Big}=($self->{F_Big}/$self->{E_Big});}
#	if (eval($self->{G_Big}->bstr) eq 0) {$self->{GdE_Big}=Math::BigFloat->bzero;} else {$self->{GdE_Big}=($self->{G_Big}/$self->{E_Big});}
# this is iffy, because any non-number string plus zero is also zero, I think
	if (0 + $self->{B_Big}->bstr eq 0) {$self->{BdA_Big}=Math::BigFloat->bzero;} else {$self->{BdA_Big}=($self->{B_Big}/$self->{A_Big});}
	if (0 + $self->{C_Big}->bstr eq 0) {$self->{CdA_Big}=Math::BigFloat->bzero;} else {$self->{CdA_Big}=($self->{C_Big}/$self->{A_Big});}
	if (0 + $self->{F_Big}->bstr eq 0) {$self->{FdE_Big}=Math::BigFloat->bzero;} else {$self->{FdE_Big}=($self->{F_Big}/$self->{E_Big});}
	if (0 + $self->{G_Big}->bstr eq 0) {$self->{GdE_Big}=Math::BigFloat->bzero;} else {$self->{GdE_Big}=($self->{G_Big}/$self->{E_Big});}
	$self->{Am3_Big}=$self->{A_Big} * 3;
	$self->{Bm2_Big}=$self->{B_Big} * 2;
	$self->{Em3_Big}=$self->{E_Big} * 3;
	$self->{Fm2_Big}=$self->{F_Big} * 2;
	$self->{Am6_Big}=$self->{A_Big} * 6;
	$self->{Em6_Big}=$self->{E_Big} * 6;
	$self->{bigInitted}=1;
	}


sub getInfiniteSlopeThetas {
	my $self = shift;
	my @zts = $self->solveXPrimeforTheta(0);
	return wantarray ? @zts:$zts[0];
	}

sub getLength { #approx length
	my $self = shift;
	my $res = shift;
	my $start_theta = shift;
	my $end_theta = shift;
	if (!defined($res)) {$res=1000;}
	if (!defined($start_theta)) {$start_theta=0;}
	if (!defined($end_theta)) {$end_theta=1;}
	if ($end_theta<$start_theta) {my $tmp=$start_theta;$start_theta=$end_theta;$end_theta=$tmp;}
	if ($end_theta == $start_theta) {return 0;}
	my $length=0;
	my $inc = ($end_theta - $start_theta)/$res;
	#print "	 $start_theta + $inc , l; $end_theta , inc;$inc\n";
	for (my $t=$start_theta + $inc;$t<=$end_theta;$t+=$inc) {
		$length+=sqrt(($self->bezierEvalXofT($t) - $self->bezierEvalXofT($t - $inc))**2 + ($self->bezierEvalYofT($t) - $self->bezierEvalYofT($t - $inc))**2);
		}
	return $length;
	}
sub precision {
	my $self=shift;
	if (defined($_[0])) {
		$self->{precision}=$_[0];
		#$self->{thetaprecision} = $self->{precision}/$self->{maxcoordval};
		}
	return $self->{precision};
	}
sub getRange {
	my $self = shift;
	return ($self->{minx},$self->{miny},$self->{maxx},$self->{maxy});
	}
sub inRange {
	my $self = shift;
	my $coords = shift;
	my $xok=0;
	my $yok=0;
	#I've spent too much time here trying to handle those boundary cases.
	#I thought I had it handled, but now I find I've been using BigFloats in the min/max x/y comparisons
	# and that this code is taking maybe half of my execution time.
	#So, lets use normal numbers to speed stuff up, unless were really close to a boundary, and then we'll let the BigFloats in.
#print "looking for in range $coords->[0] , $coords->[1] \n";
	if (defined($coords->[0])) {
		if ( #really close to a boundary - allow any BigFloats to stay big
			#1 ||
			abs($coords->[0] - $self->{minx_notbig}) < 0.0001 ||
			abs($coords->[0] - $self->{maxx_notbig}) < 0.0001
			) {
# SPEED ATTEMPTS - leave these evals here for now since hopefully this is rare?
# not so rare
            #warn "inrange using bigs 1\n";
			if ((eval($self->{minx})       < eval($coords->[0]) || eval($self->{minx})       eq eval($coords->[0])) && (eval($self->{maxx})       > eval($coords->[0]) || eval($self->{maxx})       eq eval($coords->[0]))) {
				$xok=1;
				}
			}
		else { #use definitely PERL size normal numbers for min/max x/y
			if ((    $self->{minx_notbig} < $coords->[0]        ||      $self->{minx_notbig} eq      $coords->[0])  && (     $self->{maxx_notbig} >      $coords->[0]  ||      $self->{maxx_notbig} eq      $coords->[0] )) {
				$xok=1;
				}
			}
		}
	if (defined($coords->[1])) {
		if (
			#1 ||
			abs($coords->[1] - $self->{miny_notbig}) < 0.0001 ||
			abs($coords->[1] - $self->{maxy_notbig}) < 0.0001
			) {
			#print " uuuuuuubug\n";
			#print "    test1: eval($self->{miny})       < eval($coords->[1]) ?: ",((eval($self->{miny})       < eval($coords->[1]))?'yes':'no'),"\n";
			#print "  ||test2: eval(sprintf(\"%.14f\",$self->{miny}))        eq eval($coords->[1])) ?: ",((eval(sprintf("%.14f",$self->{miny}))        eq eval($coords->[1]))?'yes':'no'),"\n";
			#print "  &&\n";
			#print "    test3:    eval($self->{maxy})       > eval($coords->[1]) ?: ",((eval($self->{maxy})       > eval($coords->[1]))?'yes':'no'),"\n";
			#print "  ||test4:    eval($self->{maxy})       eq eval($coords->[1]) ?: ",((eval($self->{maxy})       eq eval($coords->[1]))?'yes':'no'),"\n";
			#sprintf in here is a recent hack that might get to stay if it doesn't cause other problems - "%.14f" worked for awhile, then had to drop it to "%.13f" to nail a later case of similar problem of a ~0 eq 0 situation where ~0 really should have been understood or calculated as zero. When will I rewrite to whole system with exact math and "robust predicates". After the Mac Aurthur Fellowship award, right? Is that spelled correctly?
# SPEED ATTEMPTS - leave these evals here for now since hopefully this is rare?
#warn "inrange using bigs 2\n";
			if ( (eval($self->{miny})       < eval($coords->[1]) || eval(sprintf("%.13f",$self->{miny}))        eq eval($coords->[1])) && (eval($self->{maxy})       > eval($coords->[1]) || eval($self->{maxy})       eq eval($coords->[1]))) {
				$yok=1;
				}
			}
		else { #use definitely PERL size normal numbers for min/max x/y
			if ( (     $self->{miny_notbig} <      $coords->[1]  ||                      $self->{miny_notbig}   eq      $coords->[1] ) && (     $self->{maxy_notbig} >      $coords->[1]  ||      $self->{maxy_notbig} eq      $coords->[1] )) {
				$yok=1;
				}
			}
		}
	return $xok,$yok;
	}
sub onSegment {
	my $self = shift;
	my $point= shift;
	return scalar(grep {$point->[1] - $_[0]->bezierEvalYofT($_) < $self->{precision}} $self->solveXforTheta($point->[0]));
	}


# dont like that we're making so many copies of two subroutines in getFeet
# try to make those once - but then need a way to get x and y params in them
# so going to put those on self - or could be package globals

sub find90 ($) { #dotproduct equals zero for two perpendicular vectors
#		return  ($Math::MikePath::BezierCubicSegment::activeseg->bezierEvalYofT($_[0]) - $Math::MikePath::BezierCubicSegment::activey)
#		      *  $Math::MikePath::BezierCubicSegment::activeseg->slopeTangent_byTheta($_[0])
#		      + ($Math::MikePath::BezierCubicSegment::activeseg->bezierEvalXofT($_[0]) - $Math::MikePath::BezierCubicSegment::activex); # * $tanvec->[1] ( which is == 1 )

        # unrolled bezier evals to do dot product math
		return  (  (((($Math::MikePath::BezierCubicSegment::activeseg->{E} * $_[0]) + $Math::MikePath::BezierCubicSegment::activeseg->{F}) * $_[0] + $Math::MikePath::BezierCubicSegment::activeseg->{G}) * $_[0] + $Math::MikePath::BezierCubicSegment::activeseg->{H})
		         - $Math::MikePath::BezierCubicSegment::activey )
		      *  (  (($Math::MikePath::BezierCubicSegment::activeseg->{Em3} * $_[0]  +  $Math::MikePath::BezierCubicSegment::activeseg->{Fm2}) * $_[0] + $Math::MikePath::BezierCubicSegment::activeseg->{G})
		          / (($Math::MikePath::BezierCubicSegment::activeseg->{Am3} * $_[0]  +  $Math::MikePath::BezierCubicSegment::activeseg->{Bm2}) * $_[0] + $Math::MikePath::BezierCubicSegment::activeseg->{C})
		         )
		      + ( (((($Math::MikePath::BezierCubicSegment::activeseg->{A} * $_[0]) + $Math::MikePath::BezierCubicSegment::activeseg->{B}) * $_[0] + $Math::MikePath::BezierCubicSegment::activeseg->{C}) * $_[0] + $Math::MikePath::BezierCubicSegment::activeseg->{D})
		         - $Math::MikePath::BezierCubicSegment::activex); # * $tanvec->[1] ( which is == 1 )


		}
sub find90_other ($) { #dotproduct equals zero for two perpendicular vectors
        # don't bother to unroll this one - it rarely gets used

		return  ($Math::MikePath::BezierCubicSegment::activeseg->bezierEvalXofT($_[0]) - $Math::MikePath::BezierCubicSegment::activex)
		      *  $Math::MikePath::BezierCubicSegment::activeseg->slopeNormal_byTheta($_[0])
		      + -1 * ( $Math::MikePath::BezierCubicSegment::activeseg->bezierEvalYofT($_[0]) - $Math::MikePath::BezierCubicSegment::activey ); # * $tanvec->[1] ( which is == 1 )

		}



sub getFeet {
	my $self=shift;
	my $x=shift;
	my $y=shift;
	#for each interval between critical points - critical due to features of x(i) and y(i). - hueristic and then root find to get any 90 degree intersections
	my @feet=();
	my %ds={};

# clean up the pre-speed-optimized mess after you've seen to real
# geometry output
# cut point lookup time roughly in half so far, mostly with changes here

#	my @dangers=sort {$a<=>$b} grep {!$ds{$_}++} map {ref($_)?0 + sprintf("%.20f",$_->bstr):$_} (@{$self->{extremeys_is}},@{$self->{extremexs_is}});
    # let's make non-BigFloat versions of extremeis during new(), so we don't
    # ever have to call bstr in here.
    # Oh nice - cuts point lookup time by 15%.
	#my @dangers=sort {$a<=>$b} grep {!$ds{$_}++} (@{$self->{extreme_is_sorted_notbig}});

    # We feel funny setting temp package variables
    # but this lets us define the two subroutines that use these
    # just once. Previously we created anonymous subroutines right here, on
    # every call to getFeet(). That didn't seem right either, making thousands
    # of subroutines.
    # (Originally this was about looking for speed ups, but this way seems
    # to take the same amount of exe time.)

    $Math::MikePath::BezierCubicSegment::activeseg=$self;
    $Math::MikePath::BezierCubicSegment::activex=$x;
    $Math::MikePath::BezierCubicSegment::activey=$y;

	for (my $i=1;$i<scalar(@{$self->{extreme_is_sorted_notbig}});$i++) {
		my $boundl=$self->{extreme_is_sorted_notbig}->[$i - 1];
		my $boundr=$self->{extreme_is_sorted_notbig}->[$i];

		#my $st1=$self->slopeTangent_byTheta($boundl);
		#my $st2=$self->slopeTangent_byTheta($boundr);
		# maybe shaves a little time off
        my $st1=$self->{tangents_at_sorted_extremes}->[$i - 1];
        my $st2=$self->{tangents_at_sorted_extremes}->[$i];

        my $functouse = (abs($st1) > 1000 || abs($st2) > 1000
                         #|| $st1 =~ /inf/ || $st2 =~ /inf/
                         # is this faster than pattern matches?
                         || $st1 eq 'inf' || $st1 eq '-inf'
                         || $st2 eq 'inf' || $st2 eq '-inf'
                        )
                      ? \&find90_other
                      : \&find90;

		my ($foot_i,$msg);
		# Precision here is pretty high. Slight speedup if you turn it down.
		# But how much is enough? (Too much is enough.)
		($foot_i,$msg)=BrentsMethod($functouse,[$boundl,$boundr],$self->{precision}/1000,undef,'trying to find feet on Bezier in MikePath');
		if (!defined($msg)) {
			if (ref($foot_i)) { #then downgrade
# SPEED ATTEMPTS
#				$foot_i=eval(substr($foot_i->bstr,0,25));
				$foot_i=0 + sprintf("%.20f",$foot_i->bstr);
				}
			push(@feet,[$self->bezierEvalXofT($foot_i),$self->bezierEvalYofT($foot_i),$foot_i]);
			}
		else {
			# no foot found - possible, okay.
			}
		}
	return @feet;
	}
sub f { # f(x) = y
	my $self=shift;
	my $x=shift;
	my $origx=$x;#in case we downgrade x from a BigFloat, but then decide we still want to try using the BigFloat version
	my $needstobebig=0;
	for (my $i=0;$i< @{$self->{'xofidangerranges'}};$i++) {
        next if ($x <  $self->{'xofidangerranges'}->[$i]->[0]);
        next if ($x >  $self->{'xofidangerranges'}->[$i]->[1]);
		if ( (   $x >  $self->{'xofidangerranges'}->[$i]->[0]
		      && $x <  $self->{'xofidangerranges'}->[$i]->[1]   )
		   ||    $x eq $self->{'xofidangerranges'}->[$i]->[0]
		   ||    $x eq $self->{'xofidangerranges'}->[$i]->[1]
		   ) {
			$needstobebig=1;
			last;
    		}
		}
	if ($needstobebig && !ref($x) && $Math::MikePath::enableCarefulfofx) {
		$x=Math::BigFloat->new(''.$x) if !ref($x);
		}
	elsif (!$needstobebig && ref($x)) {
		#print "NOT danger zone for f(x)! so downgrading x:$x\n";
		#$x=eval("$x");
		}
	my %dupsieve;
	my @ret;
	if ((ref($x) && !$self->{isLite})) {
		@ret = map {$self->bezierEvalYofTBig($_)} grep {!$dupsieve{ref($_)?$_->bstr:$_}++} $self->solveXforThetaBig($x);
		}
	else {
		@ret = map {$self->bezierEvalYofT($_)}    grep {!$dupsieve{$_}++}       $self->solveXforTheta($x);
		}
	if (!scalar(@ret)) {
		#print "nothing for f() - is x w/in range?: (($x>$self->{minx} || $x eq $self->{minx}) && ($x<$self->{maxx} || $x eq $self->{maxx})) : ",(($x>$self->{minx} || $x eq $self->{minx}) && ($x<$self->{maxx} || $x eq $self->{maxx})?'yes':'no'),"\n";
		if (($origx>$self->{minx} || $origx eq $self->{minx}) && ($origx<$self->{maxx} || $origx eq $self->{maxx})) {
			#@ret should have had something - try again somehow
			#print "###### ret should have had something for $origx : try again somehow - \n";
			#print "  (paths minx: $self->{minx})\n";
			#print "  (paths maxx: $self->{maxx})\n";
			#print "  (paths miny: $self->{miny})\n";
			#print "  (paths maxy: $self->{maxy})\n";
			#print "bez spec: \n[ $self->{p1}->[0], $self->{p1}->[1] ],\n[ $self->{cp1}->[0], $self->{cp1}->[1] ],\n[ $self->{cp2}->[0], $self->{cp2}->[1] ],\n[ $self->{p2}->[0],  $self->{p2}->[1] ] \n";
			#print "      or: C$self->{p1}->[0],$self->{p1}->[1],$self->{cp1}->[0],$self->{cp1}->[1],$self->{cp2}->[0], $self->{cp2}->[1],$self->{p2}->[0],$self->{p2}->[1]\n";
			#maybe you hit exactly on an endpoint?
			foreach my $cp ($self->{p1},$self->{p2}) {if ($cp->[0] eq $x) {push(@ret,$cp->[1]);print "used exact control point y\n";}}
			#maybe you hit exactly on minx or maxx?
			if ($x eq $self->{maxx} || $x eq $self->{minx}) { # already know we're not at end points, so should be at extreme or peak of curve
	#warn "temp dis new hit extreme in f(x)";
			#warn "snap to extreme not at endpoint in f(x)";
				my @xPrimeZeros=$self->solveXPrimeforTheta(0);
				if (scalar(@xPrimeZeros)>=1) {
					#if multiple thetas, sort by distance from $x. Theta we want should give shortest dist, ideally zero
					@xPrimeZeros = sort {abs($x - $self->bezierEvalXofT( $a )) <=> abs($x - $self->bezierEvalXofT( $b ))} @xPrimeZeros;
					push(@ret,$self->bezierEvalYofT( $xPrimeZeros[0] ));
					}
				}
			#then, if that didn't work, try some more
			if (!scalar(@ret)) {
				print "###### ret should have had something for $origx : try again somehow - \n";
				if (!$self->{bigInitted}) {
					print "was lite, so initted bigs and tried big evals\n";
					$self->initBigs();
					$self->initDangerRanges();
					$self->{isLite}=0;
					}
				#now try again, using BigFloats all around, with original x (which might have been a BigFloat that we downgraded, since we're not always right about downgrading)
				my @ret_is = grep {!$dupsieve{ref($_)?$_->bstr:$_}++} $self->solveXforThetaBig(ref($origx)?$origx:Math::BigFloat->new(''.$origx));
				for (my $ti=0;$ti<@ret_is;$ti++) {
					my $needstobebig=0;
					for (my $i=0;$i<scalar(@{$self->{mxidangerranges}});$i++) {
						print "x(i) test:  ($ret_is[$ti] > $self->{'mxidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'mxidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[1])\n";
						if (($ret_is[$ti] > $self->{'mxidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'mxidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[1])) {
							#print "DANGER ZONE FOR x(i)! t:$ret_is[$ti]\n";
							$needstobebig=1;
							last;
							}
						}
					for (my $i=0;$i<scalar(@{$self->{myidangerranges}});$i++) {
						print "y(i) test:  ($ret_is[$ti] > $self->{'myidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'myidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[1])\n";
						if (($ret_is[$ti] > $self->{'myidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'myidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[1])) {
							#print "DANGER ZONE FOR y(i)! t:$ret_is[$ti]\n";
							$needstobebig=1;
							last;
							}
						}
					if (!$needstobebig) {
						#print 'DOWNGRADED A THETA',"\n";
						#$ret_is[$ti]=eval(substr($ret_is[$ti]->bstr,0,25));
						}

					}
				#@ret= map {ref($_)?$self->bezierEvalYofTBig($_):$self->bezierEvalYofT($_)} @ret_is;
				@ret= map {$self->bezierEvalYofTBig($_)} @ret_is;
				}

			if (!scalar(@ret)) {
				my $brute = sub {
					return $origx - $self->bezierEvalXofTBig($_[0]);
					};
				for (my $i=1;$i<@{$self->{extremexs_is}};$i++) {
					my $bl=$self->{extremexs_is}->[$i - 1];
					my $br=$self->{extremexs_is}->[$i];
					my $er;
					my $onetheta;
					($onetheta,$er) = Bisection($brute,[$bl,$br],0.0000001,($bl+$br)/2,'Brute Bisection to find at least one theta');
					if (!defined($er)) {
						push(@ret,$self->bezierEvalYofTBig($onetheta));
						print "\nBrute found $onetheta\n\n";
						}
					else {
						#print "Brute couldn't find anything either\n";
						}
					}
				}

			if (!scalar(@ret)) {
				warn "you should have found something for f(x) right?";
				}
			}
		}
	return wantarray?@ret:$ret[0];
	#return @ret;
	}
sub F { # F(y) = x
	my $self=shift;
	my $y=shift;
	my $origy=ref($y)?$y->copy():$y;
	my $needstobebig=0;
	for (my $i=0;$i<scalar(@{$self->{'yofidangerranges'}});$i++) {
		if (($y > $self->{'yofidangerranges'}->[$i]->[0] || $y eq $self->{'yofidangerranges'}->[$i]->[0]) && ($y < $self->{'yofidangerranges'}->[$i]->[1] || $y eq $self->{'yofidangerranges'}->[$i]->[1])) {
			#print "DANGER ZONE FOR F(y)! y:$y\n";
			$needstobebig=1;
			last;
			}
		}
	if ($needstobebig && !ref($y) && $Math::MikePath::enableCarefulFofy) {
		$y=Math::BigFloat->new(''.$y) if !ref($y);
		}
	else {
		#downgrade?
		}
	my %dupsieve;
	my @ret;
	if (ref($y) && !$self->{isLite}) {
		@ret = map {$self->bezierEvalXofTBig($_)} grep {!$dupsieve{ref($_)?$_->bstr:$_}++} $self->solveYforThetaBig($y);
		}
	else {
		@ret=map {$self->bezierEvalXofT($_)} grep {!$dupsieve{$_}++} $self->solveYforTheta($y);
		}
	if (!scalar(@ret)) {
		#print "nothing for F() - is y w/in range?: (($y>$self->{miny} || $y eq $self->{miny}) && ($y<$self->{maxy} || $y eq $self->{maxy})) : ",(($y>$self->{miny} || $y eq $self->{miny}) && ($y<$self->{maxy} || $y eq $self->{maxy})?'yes':'no'),"\n";
		if (($origy>$self->{miny} || eval("$origy") eq eval("$self->{miny}")) && ($origy<$self->{maxy} || eval("$origy") eq eval("$self->{maxy}"))) {
			#@ret should have had something - try again somehow
			#print "###### ret should have had something for $origy : try again somehow - \n";
			#print "  (paths minx: $self->{minx})\n";
			#print "  (paths maxx: $self->{maxx})\n";
			#print "  (paths miny: $self->{miny})\n";
			#print "  (paths maxy: $self->{maxy})\n";

			#maybe you hit exactly on an endpoint?
			foreach my $cp (($self->{p1},$self->{p2})) {if ($cp->[1] eq $y) {push(@ret,$cp->[0]);print "used exact control point x\n";}}
			#maybe you hit exactly on miny or maxy?
			if ($y eq $self->{maxy} || $y eq $self->{miny}) {
				my @yPrimeZeros=$self->solveYPrimeforTheta(0);
				if (scalar(@yPrimeZeros)>=1) {
					#see similar section in f(x) function above for comments
					#this is just adapted from that, swapping y for x
					@yPrimeZeros = sort {abs($y - $self->bezierEvalYofT( $a )) <=> abs($y - $self->bezierEvalYofT( $b ))} @yPrimeZeros;
					push(@ret,$self->bezierEvalXofT( $yPrimeZeros[0] ));
					#push(@ret,$self->bezierEvalXofT( $yPrimeZeros[0] ) . ' '); # hack! the appended space is to let the duplicate numbers pass through a string-compare-based duplicate filter downstream that (hopefully) sees diffrent string values for the identical number values
					# no, don't want that hack. think I'm handling single result okay, and if not, should be.
					#print "experimental F(y) shortcut, where y==maxy or miny, on probation. So is it working okay?\n";
					#print "  by the way - returned two identical points for this, where probably want to return just one - fix board.pm stuff so it will work with one point in this case, and then don't return second duplicate point here\n";
					}
				}

			#then, if that didn't work, try some more
			print "###### ret should have had something for $origy : try again somehow - \n";
			if (!scalar(@ret)) {
				if (!$self->{bigInitted}) {
					print "was lite, so initted bigs and tried big evals\n";
					$self->initBigs();
					$self->initDangerRanges();
					$self->{isLite}=0;
					}
				#now try again, using BigFloats all around, with original y (which might have been a BigFloat that we downgraded, since we're not always right about downgrading)
				my @ret_is = grep {!$dupsieve{ref($_)?$_->bstr:$_}++} $self->solveYforThetaBig(ref($origy)?$origy:Math::BigFloat->new(''.$origy));
				for (my $ti=0;$ti<@ret_is;$ti++) {
					my $needstobebig=0;
					for (my $i=0;$i<scalar(@{$self->{mxidangerranges}});$i++) {
						#print "x(i) test:  ($ret_is[$ti] > $self->{'mxidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'mxidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[1])\n";
						if (($ret_is[$ti] > $self->{'mxidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'mxidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'mxidangerranges'}->[$i]->[1])) {
							#print "DANGER ZONE FOR x(i)! t:$ret_is[$ti]\n";
							$needstobebig=1;
							last;
							}
						}
					for (my $i=0;$i<scalar(@{$self->{myidangerranges}});$i++) {
						#print "y(i) test:  ($ret_is[$ti] > $self->{'myidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'myidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[1])\n";
						if (($ret_is[$ti] > $self->{'myidangerranges'}->[$i]->[0] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[0]) && ($ret_is[$ti] < $self->{'myidangerranges'}->[$i]->[1] || $ret_is[$ti] eq $self->{'myidangerranges'}->[$i]->[1])) {
							#print "DANGER ZONE FOR y(i)! t:$ret_is[$ti]\n";
							$needstobebig=1;
							last;
							}
						}
					if (!$needstobebig) {
						#print 'DOWNGRADED A THETA',"\n";
						#$ret_is[$ti]=eval(substr($ret_is[$ti]->bstr,0,25));
						}
					}
				#@ret= map {ref($_)?$self->bezierEvalYofTBig($_):$self->bezierEvalYofT($_)} @ret_is;
				@ret= map {$self->bezierEvalXofTBig($_)} @ret_is;
				}



			if (!scalar(@ret)) {
				my $brute = sub {
					return $origy - $self->bezierEvalYofTBig($_[0]);
					};
				for (my $i=1;$i<@{$self->{extremeys_is}};$i++) {
					my $bl=$self->{extremeys_is}->[$i - 1];
					my $br=$self->{extremeys_is}->[$i];
					my $er;
					my $onetheta;
					($onetheta,$er) = Bisection($brute,[$bl,$br],0.0000001,($bl+$br)/2,'Brute Bisection to find at least one theta');
					if (!defined($er)) {
						push(@ret,$self->bezierEvalXofTBig($onetheta));
						print "\nBrute found $onetheta\n\n";
						}
					else {
						#print "Brute couldn't find anything either\n";
						}
					}
				}

			if (!scalar(@ret)) {
				warn "you should have found something for f(x) right?";
				}

			}
		}
	return wantarray?@ret:$ret[0];
	#return @ret;
	}
sub secondDerivative {
	my $self = shift;
	my $x    = shift;
	my $y    = @_?shift:undef;
	my @thetas;
#	if    (defined($x)) {@thetas = $self->solveXforTheta($x);}
	if    (defined($x)) {@thetas = $self->solveXforTheta_newself($x,2);} # gives theta AND the first and second derivs of theta with respect to x, by hacking a cubic solver
	elsif (defined($y)) {die "not yet, but s/b cut and paste easy";}
	else {return;}
	my @ret;
	foreach my $theta_and_deriv (@thetas) {

        my $theta = $theta_and_deriv->[0];
        my $thetaprime = $theta_and_deriv->[1];
        my $thetaprime2 = $theta_and_deriv->[2];

        my $yp=$self->bezierEvalYPrimeofT($theta);
        my $xp=$self->bezierEvalXPrimeofT($theta);
        my $ypp=$self->bezierEvalYDoublePrimeofT($theta);
        my $xpp=$self->bezierEvalXDoublePrimeofT($theta);

        # This matches yp/xp !
        # thatis yprime of theta * dtheta/dx = yprime of theta/xprime of theta
        my $ypofx = $self->{Em3} * $theta**2 * $thetaprime  +  $self->{Fm2} * $theta * $thetaprime + $self->{G} * $thetaprime;

        # for second derivative, have to use chain and product rules to differentiate the above.

        my $yppofx = $self->{Em3} * ($theta**2 * $thetaprime2 + 2*$theta*$thetaprime**2)
                   + $self->{Fm2} * ($theta    * $thetaprime2 +          $thetaprime**2)
                   + $self->{G}   * $thetaprime2;

        return $yppofx if (!$main::dodebug);

        # JUST PASSING BY LATER... and want to note that I think the above was/is
        # working fine when I was last deving here. And I just didn't bother to
        # clean up what's below, partly because as it says right there, there's maybe useful debug stuff
        # and partly because I just never have the mental spacetime in this environment to burnish the gems...
        # But, yeah, looks like I figured graph-space 2nd deriv for cubic bezier,
        # and I think that means I can calc graph-space curvature (K) or radius of curvature too.
        # (I used this as part of doing that for rail joiner curvature - but that had extra complicating steps.
        # In other words, calcing curvature for simple cubic bez case is probably simple, since I did curvature for something more complex...)
        # An SVG Path library that offers analytic graph-space 2nd derivitive and curvature calcs is
        # something some people will want.
        # This needs test suite and engaging documentation with app examples.
        # Okay, also, just noticing that looks like all the old calcs above for $yp,$xp,$ypp, and $xpp can go.
        # Originally thought I would be working with them, or at least comparing to them, but
        # you can see the math actually used for the return val didn't use them. (Double check! 'cause I'm just quick passing through.)


        # You'll want some of this debug mess (and more probably)
        # as you hit each of the 5 expressions in the solver - especially
        # the two Det >= 0 cases.

        # no, because thetaprime is really an expression containing x, so
        # you also want thetadoubleprimeofx now
        #

        warn "\n";
        warn "theta        : $theta\n";
        warn "1st derrrrr??: $ypofx            vs yp/xp: ",($yp/$xp),"\n";
        warn "2nd derrrrr??: $yppofx\n";
        warn "\n";


        my $hh=0.001; # approx can actually get worse with higher res hh, so careful
        my ($th1,$dr1,$dr1_2) = map @$_, $self->solveXforTheta_newself($x-$hh,2);
        my ($th2,$dr2,$dr2_2) = map @$_, $self->solveXforTheta_newself($x    ,2);
        my ($th3,$dr3,$dr3_2) = map @$_, $self->solveXforTheta_newself($x+$hh,2);
        my $tm1 = ($th2-$th1)/$hh;
        my $tm2 = ($th3-$th2)/$hh;
        my $tdx_approx = ($tm2-$tm1)/$hh;
        warn "approx test thetas : $th1 , $th2 , $th3\n";
        warn "approx test theta's : $dr1 , $dr2 , $dr3\n";
        warn "approx test theta''s : $dr1_2 , $dr2_2 , $dr3_2\n";
        warn "\n";
        warn "tPrimeOfX: 2 approx slopes vs calc tan in middle:\n";
        warn "$tm1\n$dr2 <-- calc s/b in middle\n$tm2\n";
        warn "\n";
        warn "tDoublePrimeOfX: approx vs 3 s/b bracketing calcs\n";
        warn "$tdx_approx = ($tm2 - $tm1)/$hh vs\n$dr1_2\n$dr2_2\n$dr3_2\n";

        my ($th4,$dr4) = map @$_, $self->solveXforTheta_newself($x+1,'doderiv');
        warn "ifonly: ($th2 + $dr2) = ",($th2 + $dr2)," vs $th4 hey actually close! maybe right\n";


# not quite here...
# must be of x not of i

        my $lowdeehiminushideelow = ($xp*$ypp - $yp*$xpp);
        my $lowlow = $xpp**2;

		#if ($xpp eq '0') {push(@ret,(($lowdeehiminushideelow < 0)?'-':'+').'inf');}
		#if ($lowdeehiminushideelow eq '0') {push(@ret,(($lowlow < 0)?'-':'+').'0');}

        # (yp/xp)' = low dee high minus high dee low over low low
        # (xp*ypp - yp*xpp) / xpp^2



warn "\n\nyp/xp eq? yp * dt/dx ?\n";
warn "",($yp/$xp), " eq? ",($yp*$thetaprime)," or ",$ypofx,"\n";

warn "  ypp stuff $yppofx : ",($ypp/$xpp), " eq? ",($yp*$thetaprime*$thetaprime2),"\n";

die "stop w/in 2nd deriv";
return $yppofx;


		#else {
			push(@ret, ($thetaprime*2) * ($lowdeehiminushideelow / $lowlow));
			# might it be just ?? push(@ret, $ypp / $xpp);
# looking into hodograph approach - which I might have already mimicked? in my first derivative approach? but using the cubic formula. hmmm doesn't seem like it should work
            #push @ret, $ypp / $xpp;
		#	}

		}
	#warn "ret:",join(',',@ret),"\n";
	return @ret;
	}
sub slopeTangent {
	my $self = shift;
	my $x    = shift;
	my $y    = @_?shift:undef;
	my @thetas;
	if    (defined($x)) {@thetas = $self->solveXforTheta($x);}
	elsif (defined($y)) {@thetas = $self->solveYforTheta($y);}
	else {return;}
	my @ret;
	foreach my $theta (@thetas) {
		my $yp=$self->bezierEvalYPrimeofT($theta);
		my $xp=$self->bezierEvalXPrimeofT($theta);
		if ($xp eq '0') {push(@ret,(($yp < 0)?'-':'+').'inf');}
		if ($yp eq '0') {push(@ret,(($xp < 0)?'-':'+').'0');}
		else {
			push(@ret,$yp/$xp);
			}
		}
	return @ret;
	}
sub slopeTangent_byTheta {
	my $self = shift;
	my $theta = shift;
	my $yp=$self->bezierEvalYPrimeofT($theta);
	my $xp=$self->bezierEvalXPrimeofT($theta);
	# adding zero here converts possible '-0' to 0 for this comparison
	if (0 + $xp eq '0') {return (($yp < 0)?'-':'+').'inf';}
	if (0 + $yp eq '0') {return (($xp < 0)?'-':'+').'0';}
	else {
		return $yp/$xp;
		}
	}
sub angleTangent {
	my $self = shift;
	my $x    = shift;
	my $y    = @_?shift:undef;
	my @thetas;
	if    (defined($x)) {@thetas = $self->solveXforTheta($x);}
	elsif (defined($y)) {@thetas = $self->solveYforTheta($y);}
	else {return;}
	my @ret;
	foreach my $theta (@thetas) {
		my $yp=$self->bezierEvalYPrimeofT($theta);
		my $xp=$self->bezierEvalXPrimeofT($theta);
		push(@ret,atan2($yp,$xp));
		}
	return @ret;
	}
sub angleTangent_byTheta {
	my $self = shift;
	my $theta = shift;
	my $yp=$self->bezierEvalYPrimeofT($theta);
	my $xp=$self->bezierEvalXPrimeofT($theta);
	return atan2($yp,$xp);
	}
sub fDoublePrime {
	my $self = shift;
	my $x    = shift;
	my $y    = @_?shift:undef;
	my @thetas;
	if    (defined($x)) {@thetas = $self->solveXforTheta($x);}
	elsif (defined($y)) {@thetas = $self->solveYforTheta($y);}
	else {return;}
	my @ret;
	foreach my $theta (@thetas) {
		my $yp=$self->bezierEvalYPrimeofT($theta);
		my $ypp=$self->bezierEvalYDoublePrimeofT($theta);
		my $xp=$self->bezierEvalXPrimeofT($theta);
		my $xpp=$self->bezierEvalXDoublePrimeofT($theta);
		if ($xp eq '0') {push(@ret,(($yp < 0)?'-':'+').'inf');}
		if ($yp eq '0') {push(@ret,(($xp < 0)?'-':'+').'0');}
		else {push(@ret,($xp*$ypp)-($yp*$xpp)/($xp**2));} #lo dhi minus hi dlo over lo lo, right?
		}
	return @ret;
	}
sub slopeNormal {
	my $self = shift;
	my $x    = shift;
	my $y    =@_?shift:undef;
	my @ret;
	for my $slopeTangent ($self->slopeTangent($x,$y)) {
		my $negRecip;
		if ($slopeTangent =~ /([\-\+]?)inf$/i) {
			my $sign = '';
			if (length($1)) {if ($1 eq '-') {$sign='+';} else {$sign='-';}}
			$negRecip=$sign.'0';
			}
		elsif ($slopeTangent =~ /^([\-\+]?)0$/) {
			my $sign = '';
			if (length($1)) {if ($1 eq '-') {$sign='+';} else {$sign='-';}}
			$negRecip=$sign.'inf';
			}
		else {
			$negRecip=-1/$slopeTangent;
			}
		push(@ret,$negRecip);
		}
	return @ret;
	}

sub slopeNormal_byTheta {
	my $self = shift;
	my $theta = shift;
	my $slopeTangent = $self->slopeTangent_byTheta($theta);
	if ($slopeTangent =~ /([\-\+]?)inf$/i) {
		my $sign = '';
		if (length($1)) {if ($1 eq '-') {$sign='+';} else {$sign='-';}}
		return $sign.'0';
		}
	elsif ($slopeTangent =~ /^([\-\+]?)0$/) {
		my $sign = '';
		if (length($1)) {if ($1 eq '-') {$sign='+';} else {$sign='-';}}
		return $sign.'inf';
		}
	else {
		return -1/$slopeTangent;
		}
	}
sub angleNormal {
	my $self = shift;
	my $x    = shift;
	my $y    = @_?shift:undef;
	my @thetas;
	if    (defined($x)) {@thetas = $self->solveXforTheta($x);}
	elsif (defined($y)) {@thetas = $self->solveYforTheta($y);}
	else {return;}
	my @ret;
	foreach my $theta (@thetas) {
		my $yp=$self->bezierEvalYPrimeofT($theta);
		my $xp=$self->bezierEvalXPrimeofT($theta);
		push(@ret,atan2(-$xp,$yp));
		}
	return @ret;
	}
sub angleNormal_byTheta {
	my $self = shift;
	my $theta = shift;
	my $yp=$self->bezierEvalYPrimeofT($theta);
	my $xp=$self->bezierEvalXPrimeofT($theta);
	return atan2(-$xp,$yp);
	}
sub point {
	my $self=shift;
	my $theta=shift;
	for (my $i=0;$i<scalar(@{$self->{'mxidangerranges'}});$i++) {
		if (($theta > $self->{mxidangerranges}->[$i]->[0] || $theta eq $self->{mxidangerranges}->[$i]->[0]) && ($theta < $self->{mxidangerranges}->[$i]->[1] || $theta eq $self->{mxidangerranges}->[$i]->[1]) && !ref($theta)) {
			#print "DANGER ZONE FOR point(theta) for x. $theta:ref?:",ref($theta),"\n";
			#print "DZ  theta: $theta\n";
			$theta=Math::BigFloat->new($theta) if !ref($theta);
			#print "DZB theta: $theta\n";
			}
		}
	for (my $i=0;$i<scalar(@{$self->{'myidangerranges'}});$i++) {
		if (($theta > $self->{myidangerranges}->[$i]->[0] || $theta eq $self->{myidangerranges}->[$i]->[0]) && ($theta < $self->{myidangerranges}->[$i]->[1] || $theta eq $self->{myidangerranges}->[$i]->[1]) && !ref($theta)) {
			#print "DANGER ZONE FOR point(theta) for y. $theta:$theta\n";
			$theta=Math::BigFloat->new($theta) if !ref($theta);
			}
		}
	#if (ref($theta)) {print "  your XY sir: ",$self->bezierEvalXofT($theta),",",$self->bezierEvalYofT($theta),"\n";}
	my $ret;
	if (ref($theta)) {$ret=[$self->bezierEvalXofTBig($theta),$self->bezierEvalYofTBig($theta)];}
	else {$ret=[$self->bezierEvalXofT($theta),$self->bezierEvalYofT($theta)];}
	return $ret;
	}

sub bezierEvalXofT {
	my $self = shift;
	my $t    = shift;
	return ((($self->{A} * $t) + $self->{B}) * $t + $self->{C}) * $t + $self->{D};
	}
sub bezierEvalXofTBig {
	my $self = shift;
	my $t    = shift;
	return ((($self->{A_Big} * $t) + $self->{B_Big}) * $t + $self->{C_Big}) * $t + $self->{D_Big};
	}
sub bezierEvalYofT {
	my $self = shift;
	my $t    = shift;
	return ((($self->{E} * $t) + $self->{F}) * $t + $self->{G}) * $t + $self->{H};
	}
sub bezierEvalYofTBig {
	my $self = shift;
	my $t    = shift;
	return ((($self->{E_Big} * $t) + $self->{F_Big}) * $t + $self->{G_Big}) * $t + $self->{H_Big};
	}
sub bezierEvalXPrimeofT {
	my $self = shift;
	my $t    = shift;
	# x'= 3At^2+  2Bt  +     C
	#   =(3At  +  2B)t +     C
	my $ret = ($self->{Am3} * $t  +  $self->{Bm2}) * $t + $self->{C};
	if ($ret == 0) {$ret = (($self->bezierEvalXDoublePrimeofT($t) < 0)?'-':'').$ret;} #? is that enough? useful?
	return $ret;
	}
sub bezierEvalYPrimeofT {
	my $self = shift;
	my $t    = shift;
	# y'= 3Et^2+  2Ft  +     G
	#   =(3Et  +  2F)t +     G
	my $ret = ($self->{Em3} * $t  +  $self->{Fm2}) * $t + $self->{G};
	if ($ret eq '0') {$ret = (($self->bezierEvalYDoublePrimeofT($t) < 0)?'-':'').$ret;} #? is that enough? useful?
	return $ret;
	}
sub bezierEvalXDoublePrimeofT {
	my $self = shift;
	my $t    = shift;
	# x''= 6At +  2B
	return $self->{Am6} * $t + $self->{Bm2};
	}
sub bezierEvalYDoublePrimeofT {
	my $self = shift;
	my $t    = shift;
	# y''= 6Et +  2F
	return $self->{Em6} * $t + $self->{Fm2};
	}
sub bezierSolve { #obsolete?
	my $self = shift;
	my $x    = shift;
	my $y    = shift;
	return [$self->solveX($x),$self->solveY($y)];
	}
sub solveXforTheta {
	my $self = shift;
	my $x    = shift;
	# x = At^3 +  Bt^2 +     Ct +     D
	# 0 = At^3 +  Bt^2 +     Ct +     D -x
	# 0 =  t^3 + (B/A)t^2 + (C/A)t + (D-x)/A
	#print "for cubic formula: $self->{BdA} , $self->{CdA} , ",(($self->{D} - ((ref($x))?$x:$x))/$self->{A}),"\n";
	#print "pre filter cubic formula returns:",join(" , ",&cubicformula($self->{BdA},$self->{CdA},($self->{D} - ((ref($x))?$x:$x))/$self->{A},0)),"\n";
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &cubicformula($self->{BdA},$self->{CdA},($self->{D} - ((ref($x))?$x:$x))/$self->{A},1);
	}


# Adapted from our own CubicFormula.pm
# and optimized for use with cubic Beziers -
# just real roots, some values pre-computed,
# willing to make ugly if it's faster.
# Also computes first and second derivatives.
# Would also like to dev this in direction of
# knowing numerically unstable scenarios and fixing or avoiding them,
# without, perhaps, resorting to BigFloats as much, or at all.

sub solveXforTheta_newself {
    my $self = shift;
    my $x    = shift;
    my $doThetaPrime = @_?shift:0;

    my $C = $self->{DdA} - $x/$self->{A};
    my $bigpi = (ref($self->{cubA}) || ref($self->{cubB}) || ref($C))?1:0; # boolean, whether to use BigFloat version of pi
    my $thispi=$bigpi?$pi_big:$pi;
    my $thistwopi=$bigpi?$twopi_big:$twopi;
    my $thisfourpi=$bigpi?$fourpi_big:$fourpi;

    my @inrange;
    my @deriv;
    my @deriv2;

    my ($R, $D, $X1, $X2, $X3);

    # do the full R calc here, because if you precalc some of it's parts
    # you get rounding sometimes on the last digit of those intermediates
    # and the whole result doesn't come out the same as doing it all at once
    # all together like this.
    $R = (9.0*$self->{cubA}*$self->{cubB} - 27.0*$C - 2.0*$self->{cubA}**3)/54.0;

    $D = $self->{cubQ}**3 + $R**2;                  # polynomial discriminant

    # was just reading about rationalizing to avoid subtraction
    # since subtraction can be such a hungry hippo with significant digits
    # that Q can be negative, and then that's like R^2 - Q^3
    # could you rationalize to (R^2 - Q^3)(R^2 + Q^3) / (R^2 + Q^3)
    # (R^4 - Q^6) / (R^2 + Q^3)
    # sorta think that's no good, but maybe just maybe?
    # would need tests to see what it's effect is on the typical problem areas
    # in here. look at any other subtractions in here too. Maybe we can eliminate
    # resort to BigFloats?
    # Might be better to experiment on this with a fresh copy of standalone
    # cubic solver, and set up direct tests for each problem area in these calcs,
    # prob starting with that bit a bit below where you upgrade to bigfloat
    # when abs($R - $self->{cubsqrtofnegQcubed}) < 0.000000000001.
    # that might become (R^2 - (-Q^3)) / ($R + $self->{cubsqrtofnegQcubed})
    # hokum?

    my $allReal = 1;

    if ($D > 0 || $D eq 0) { # 1 real, and 2 complex or 2 real duplicate roots
        $allReal = 0;
        my $sqrtD=(ref($D)?bigsqrt($D):sqrt($D));
        my $preS=$R + $sqrtD;
        my $preT=$R - $sqrtD;
        my $S = (($preS < 0)?-1:1) * (abs($preS)**(1/3));
        my $T = (($preT < 0)?-1:1) * (abs($preT)**(1/3));
        my $SpT = $S + $T;
        $X1 = -$self->{cubAd3} + $SpT;              # real root
        push(@inrange, $X1) if (1 > $X1 || 1 eq $X1) && ($X1 > 0 || $X1 eq 0);

        my $deriv  = (1/(6  * $sqrtD*$self->{A}))
                   * (  ($preS/(($preS**2)**(1/3)))
                      - ($preT/(($preT**2)**(1/3)))
                     );

        my $deriv2 = (1/(12 * $D    *$self->{A}**2))
                   * (  ($preS/(($preS**2)**(1/3)))
                      * ( (1/3) - $R/($sqrtD) )
                      +
                        ($preT/(($preT**2)**(1/3)))
                      * ( (1/3) + $R/($sqrtD) )
                     )
        ;

        push @deriv , $deriv;
        push @deriv2, $deriv2;

        #if ((1 > $X1 || 1 eq $X1) && ($X1 > 0 || $X1 eq 0)) {
        #    $main::dodebug=1;
        #    }



        if ($D eq 0) { # $D==0 case, duplicate real roots
            $allReal=1;
            $X2 = -$self->{cubAd3} - $SpT/2; # real part of complex root (but complex part is 0 here, so real)
warn "yall need a deerivdiv herabouts too";
warn "this right??";
        push @deriv , $deriv /2, $deriv /2;
        push @deriv2, $deriv2/2, $deriv2/2;
die "die here until ready to figure out";

            # sort as you go
            if ((1 > $X2 || 1 eq $X2) && ($X2 > 0 || $X2 eq 0)) {
                if ($X2 >= $X1) {push( @inrange, $X2, $X2);}
                else {unshift(@inrange, $X2,$X2);}
                }
            }
        }
    else {                                          # 3 distinct real roots
        # if only we could do it in one line like this
        #my $th = acos($R/sqrt(-1 * ($Q**3)));
        # but numerical instability lurks in here.

        my $toAcos;
        my $sqrtofnegQcubed = (
            ref($self->{cubQ})
            # this check is key to fixing rare numerical instability bug
            || abs($R - $self->{cubsqrtofnegQcubed}) < 0.000000000001
            )
            ? $self->cubsqrtofnegQcubed_big()
            : $self->{cubsqrtofnegQcubed};

        # upgrade R if nec. so / operator overload works with R on left, setting Big mode
        $R = Math::BigFloat->new(''.$R) if (ref($sqrtofnegQcubed) && ! ref($R));

        $toAcos = $R / $sqrtofnegQcubed;

        my $th;

        if (ref($toAcos)) {
            # this stuff with "$noom" is from debug of rare numerical instabliltiy.
            # if tempted to simplify any of this, do that in context of making all the math here robust.
            my $noom=$toAcos->copy();
            #if ($noom eq 'NaN') {die "noom is NaN after big copy from toAcos\n";}
            $noom->bpow(2)->bmul(-1.0)->badd(1.0);
            #if ($noom eq 'NaN') {die "noom is NaN after series of pow, mul, add operations\n";}

            if ($noom < 0) {
                # snap neg to zero - not sure that's good, but maybe -
                # guessing this should not ever go negative
                $noom = Math::BigFloat->bzero;
                }
            my $newnoom;
            if ($noom <= 0) {$newnoom=Math::BigFloat->bzero;}
            else {$newnoom=bigsqrt($noom);}
            #if ($newnoom eq 'NaN') {die "noom is NaN after bigsqrt. before, noom was: ",$noom->bstr,"\n";}
            #else {
                $noom = $newnoom;
            #    }
            # This arctan is not the same as atan2 or even atan - it's input is
            # limited to abs(x)<=1. Math::Big::arctan uses a slower-to-converge
            # Taylor series that is prone to not converging near 1, so I made my
            # own arctan based on some faster Euler series.
            my $abscmp=$toAcos->bacmp($noom);
            if ($abscmp < 0 || $abscmp eq 0) {
                my $quo=$toAcos->copy()->bdiv($noom);
                #if ($quo eq 'NaN') {die "pre arctan_Euler call 1: have a NaN derived from toAcos/noom : ",$toAcos->bstr," / ",$noom->bstr,"\n"}
                $th=Math::CubicFormula::arctan_Euler($quo,25);
                $th->bmul(-1)->badd($piovertwo_big);
                }
            else {
                my $quo=$noom->copy()->bdiv($toAcos);
                #if ($quo eq 'NaN') {die "pre arctan_Euler call 2: have a NaN derived from noom/toAcos : ",$noom->bstr," / ",$toAcos->bstr,"\n"}
                $th=Math::CubicFormula::arctan_Euler($quo,25);
                }
            $th = Math::CubicFormula::atanfix($th,$toAcos,$noom);
            }
        else {
            #   arccosine
            $th=atan2(sqrt(1 - $toAcos * $toAcos),$toAcos);

            # arctan_Euler() might be called for here sometimes too
            # could that make it robust while using all doubles maybe?



            }

        # think you can work out the $th range that works
        # and then grep the three ($th+C)/3 angles so you don't always have to
        # calculate all three roots below.Often a 60% speed up for this section, right?
        # Prob not. but should register.

        if ($bigpi || ref($th) || ref($sqrtofnegQcubed) || ref($R)) { # was testing $A too, but
                                                        # bigpi should be true if ref($A) is true
                                                        # - probably true for (some of) the others too, but check
            if (ref($sqrtofnegQcubed)) {
                my $cubtwotimessqrtofnegQ_big = $self->cubtwotimessqrtofnegQ_big();
                $X1 = $cubtwotimessqrtofnegQ_big * Math::Big::cos( $th               /3,40) - $self->{cubAd3};
                $X2 = $cubtwotimessqrtofnegQ_big * Math::Big::cos(($th + $thistwopi )/3,40) - $self->{cubAd3};
                $X3 = $cubtwotimessqrtofnegQ_big * Math::Big::cos(($th + $thisfourpi)/3,40) - $self->{cubAd3};
                push @inrange, grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} ($X1, $X2, $X3);
                }
            else {
                $X1 = $self->{cubtwotimessqrtofnegQ}     * Math::Big::cos( $th               /3,40) - $self->{cubAd3};
                $X2 = $self->{cubtwotimessqrtofnegQ}     * Math::Big::cos(($th + $thistwopi )/3,40) - $self->{cubAd3};
                $X3 = $self->{cubtwotimessqrtofnegQ}     * Math::Big::cos(($th + $thisfourpi)/3,40) - $self->{cubAd3};
                push @inrange, grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} ($X1, $X2, $X3);
                }
            }

            # OPTIMIZE this else, then copy approach to above two versions of same


        else {
            #my $twotimessqrtofnegQ=2*sqrt(-1 * $Q);

            # if it's more often not in range, this speeds things up
            # if it's usually in range this slows it down

            # The range you should really precalc is the input x range
            # like the C range, I guess.
            # Maybe you work back to that?
            # Find upstream ranges from the range you now have.
            # Go a step back in theta calc, and figure corresponding ranges for that.
            #toAcos might be first point for that

            #my $cos1=cos($th/3);
            #push(@inrange, $self->{cubtwotimessqrtofnegQ} * $cos1 - $self->{cubAd3}) if ($cos1 <= $self->{cubtheta_slope_upper_bound} && $cos1 >= $self->{cubtheta_slope_lower_bound});
            #my $cos2=cos(($th + $thistwopi)/3);
            #push(@inrange, $self->{cubtwotimessqrtofnegQ} * $cos2 - $self->{cubAd3}) if ($cos2 <= $self->{cubtheta_slope_upper_bound} && $cos2 >= $self->{cubtheta_slope_lower_bound});
            #my $cos3=cos(($th + $thisfourpi)/3);
            #push(@inrange, $self->{cubtwotimessqrtofnegQ} * $cos3 - $self->{cubAd3}) if ($cos3 <= $self->{cubtheta_slope_upper_bound} && $cos3 >= $self->{cubtheta_slope_lower_bound});
=cut
            my $cos1=cos($th/3);
            if ($cos1 <= $self->{cubtheta_slope_upper_bound} && $cos1 >= $self->{cubtheta_slope_lower_bound}) {
                my $root = $self->{cubtwotimessqrtofnegQ} * $cos1 - $self->{cubAd3};
                push(@inrange, $root) if (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));

                }
            my $cos2=cos(($th + $thistwopi)/3);
            if ($cos2 <= $self->{cubtheta_slope_upper_bound} && $cos2 >= $self->{cubtheta_slope_lower_bound}) {
                my $root = $self->{cubtwotimessqrtofnegQ} * $cos2 - $self->{cubAd3};
                push(@inrange, $root) if (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                }
            my $cos3=cos(($th + $thisfourpi)/3);
            if ($cos3 <= $self->{cubtheta_slope_upper_bound} && $cos3 >= $self->{cubtheta_slope_lower_bound}) {
                my $root = $self->{cubtwotimessqrtofnegQ} * $cos3 - $self->{cubAd3};
                push(@inrange,$root) if (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                }
=cut



            # ifs as blocks instead of at end of push line does not slow things
            # down significantly
            # if conditions hopefully are permissive around the edges - checking
            # if theta is (not outside) the limits rather than (inside || on)
            # Even when checking inside or on, you get false positives that you have to weed out
            # with the if on the push line. So if that has to be there anyway
            # avoid the >||eq slowness, and don't worry about a few false positives getting in.


            my $tocos1=$th;
            #warn "$self->{cubtheta_upper_bound1} > $tocos1 : ",($self->{cubtwotimessqrtofnegQ} * cos($tocos1/3) - $self->{cubAd3})," > $self->{cubtheta_lower_bound1}\n";
            if (1 || !($tocos1 > $self->{cubtheta_upper_bound1}) && !($tocos1 < $self->{cubtheta_lower_bound1})) {
                my $root = $self->{cubtwotimessqrtofnegQ} * cos($tocos1/3) - $self->{cubAd3};
                push(@inrange, $root) if (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));


                my $deriv =
                    ($self->{cubsqrtofnegQ}/(3*$self->{A}))
                    * (sin($tocos1/3) / (sqrt(-$D)))
                ;
                my $deriv2 =
                    -1 * # because I didn't keep good enough track of the square root of -1 while deriving this
                    ($self->{cubsqrtofnegQ}/(6 * $D * $self->{A}**2))
                    *
                    ( (sin($tocos1/3) / sqrt(-$D))  * $R - cos($tocos1/3)/3 )
                ;



                push(@deriv, $deriv) if $doThetaPrime && (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                push(@deriv2, $deriv2) if $doThetaPrime==2 && (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                }
            my $tocos2=($th + $thistwopi);
            #warn "$self->{cubtheta_upper_bound2} > $tocos2 : ",($self->{cubtwotimessqrtofnegQ} * cos($tocos2/3) - $self->{cubAd3})," > $self->{cubtheta_lower_bound2}\n";
            if (1 || !($tocos2 > $self->{cubtheta_upper_bound2}) && !($tocos2 < $self->{cubtheta_lower_bound2})) {
                my $root = $self->{cubtwotimessqrtofnegQ} * cos($tocos2/3) - $self->{cubAd3};
                push(@inrange, $root) if (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));


                my $deriv =
                    ($self->{cubsqrtofnegQ}/(3*$self->{A}))
                    * (sin($tocos2/3) / (sqrt(-$D)))
                ;
                my $deriv2 =
                    -1 * # because I didn't keep good enough track of the square root of -1 while deriving this
                    ($self->{cubsqrtofnegQ}/(6 * $D * $self->{A}**2))
                    *
                    ( (sin($tocos2/3) / sqrt(-$D))  * $R - cos($tocos2/3)/3 )
                ;


                push(@deriv, $deriv) if $doThetaPrime && (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                push(@deriv2, $deriv2) if $doThetaPrime==2 && (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                }
            my $tocos3=($th + $thisfourpi);
            #warn "$self->{cubtheta_upper_bound3} > $tocos3 : ",($self->{cubtwotimessqrtofnegQ} * cos($tocos3/3) - $self->{cubAd3})," > $self->{cubtheta_lower_bound3}\n";
            if (1 || !($tocos3 > $self->{cubtheta_upper_bound3}) && !($tocos3 < $self->{cubtheta_lower_bound3})) {
                my $root = $self->{cubtwotimessqrtofnegQ} * cos($tocos3/3) - $self->{cubAd3};
                push(@inrange, $root) if (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));

                my $deriv =
                    ($self->{cubsqrtofnegQ}/(3*$self->{A}))
                    * (sin($tocos3/3) / (sqrt(-$D)))
                ;
                my $deriv2 =
                    -1 * # because I didn't keep good enough track of the square root of -1 while deriving this
                    ($self->{cubsqrtofnegQ}/(6 * $D * $self->{A}**2))
                    *
                    ( (sin($tocos3/3) / sqrt(-$D))  * $R - cos($tocos3/3)/3 )
                ;
                #warn "\n root deriv: $deriv [$self->{cubtwotimessqrtofnegQ} , $R / $sqrtofnegQcubed , $self->{A}]\n";
                push(@deriv, $deriv) if $doThetaPrime && (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                push(@deriv2, $deriv2) if $doThetaPrime==2 && (($root > 0 || $root eq 0) && ($root < 1 || $root eq 1));
                }

            }
#die;

        }


    if ($doThetaPrime) {

        # returns list of array refs instead of scalars

        my @ret_inds;
        # three item sort, because we think "sort {...} @ret" is expensive
        # hmm for this one - is this still faster than sort? maybe
        splice(@ret_inds,$#ret_inds==-1?0:($inrange[$_]<$inrange[$ret_inds[0]]?0:($inrange[$_]>$inrange[$ret_inds[-1]]?$#ret_inds+1:$#ret_inds)),0,$_)
            for (0 .. $#inrange);
        if ($doThetaPrime == 2) {
            return map {[$inrange[$_], $deriv[$_], $deriv2[$_]]} @ret_inds;
            }
        else {
            return map {[$inrange[$_], $deriv[$_]]             } @ret_inds;
            }
        }
    elsif ($allReal) { # up to three real roots
        my @ret;
        # three item sort, because we think "sort {...} @ret" is expensive
        splice(@ret,$#ret==-1?0:($_<$ret[0]?0:($_>$ret[-1]?$#ret+1:$#ret)),0,$_)
            for @inrange;
        return @ret;
       }
    else { # one real root, when D > 0
        return ($inrange[0]);
        }

	}


sub solveXforThetaBig {
	my $self = shift;
	my $x    = shift;
	# x = At^3 +  Bt^2 +     Ct +     D
	# 0 = At^3 +  Bt^2 +     Ct +     D -x
	# 0 =  t^3 + (B/A)t^2 + (C/A)t + (D-x)/A
	#print "for BIG cubic formula: $self->{BdA_Big}  ,  $self->{CdA_Big}, ",$self->{D_Big}->copy()->bsub($x)->bdiv($self->{A_Big})->bstr,"\n";
	#print "pre filter cubic formula          returns:",join(" , ",&cubicformula($self->{BdA}    ,$self->{CdA}    ,($self->{D}     - ((ref($x))?$x:$x))/$self->{A}    ,0)),"\n";
	#print "pre filter cubic formula with BIG returns:",join(" , ",&cubicformula($self->{BdA_Big},$self->{CdA_Big},($self->{D_Big} - ((ref($x))?$x:$x))/$self->{A_Big},0)),"\n";
	#print "going BIGGGGGGG with x: $x\n";
    my $cubic_solve_C = $self->{D_Big}->copy();
    $cubic_solve_C->bsub($x)->bdiv($self->{A_Big});
	return sort {$a<=>$b} grep {warn "big: ",$_,"\n"; (1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &cubicformula($self->{BdA_Big},$self->{CdA_Big},$cubic_solve_C,1);
	}
sub solveYforTheta {
	my $self = shift;
	my $y    = shift;
	# y = Et^3 +  Ft^2 +     Gt +     H
	# 0 = Et^3 +  Ft^2 +     Gt +     H - y
	# 0 =  t^3 + (F/E)t^2 + (G/E)t + (H - y)/E
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &cubicformula($self->{FdE},$self->{GdE},($self->{H} - ((ref($y))?$y:$y))/$self->{E},1);
	}
sub solveYforThetaBig {
	my $self = shift;
	my $y    = shift;
	# y = Et^3 +  Ft^2 +     Gt +     H
	# 0 = Et^3 +  Ft^2 +     Gt +     H - y
	# 0 =  t^3 + (F/E)t^2 + (G/E)t + (H - y)/E
	#my @ts = &cubicformula($self->{FdE_Big},$self->{GdE_Big},($self->{H_Big} - ((ref($y))?$y:$y))/$self->{E_Big},1);
	#print " ts: ",join(", ",@ts),"\n";
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &cubicformula($self->{FdE_Big},$self->{GdE_Big},($self->{H_Big} - ((ref($y))?$y:$y))/$self->{E_Big},1);
	}
sub solveXPrimeforTheta { # x's slope related to theta (not y)
	my $self = shift;
	my $xp    = shift;
	# x'= 3At^2+  2Bt  +     C
	# 0 = 3At^2+  2Bt  +     C - x'
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &quadraticformula($self->{Am3},$self->{Bm2},$self->{C} - ((ref($xp))?$xp:$xp),1);
	}
sub solveXPrimeforThetaBig { # x's slope related to theta (not y)
	my $self = shift;
	my $xp    = shift;
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &quadraticformula($self->{Am3_Big},$self->{Bm2_Big},$self->{C_Big} - ((ref($xp))?$xp:$xp),1);
	}
sub solveYPrimeforTheta {
	my $self = shift;
	my $yp    = shift;
	# y'= 3Et^2+  2Ft  +     G
	# 0 = 3Et^2+  2Ft  +     G - y'
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &quadraticformula($self->{Em3},$self->{Fm2},$self->{G} - ((ref($yp))?$yp:$yp),1);
	}
sub solveYPrimeforThetaBig {
	my $self = shift;
	my $yp    = shift;
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &quadraticformula($self->{Em3_Big},$self->{Fm2_Big},$self->{G_Big} - ((ref($yp))?$yp:$yp),1);
	}
sub solvefprimefortheta { #obsolete? since this is just slopeTangent(x) ?
	my $self = shift;
	my $fp    = shift;
	# f'= y'/x' = (3Et^2 + 2Ft + G) / (3At^2 + 2Bt + C)
	# 0 = 3(E-f'A)t^2 + 2(F-f'B)t + (G-f'C)
	return sort {$a<=>$b} grep {(1 > $_ || 1 eq $_) && ($_ > 0 || $_ eq 0)} &quadraticformula($self->{Em3} - $fp * $self->{Am3},$self->{Fm2} - $fp * $self->{Bm2},$self->{G} - $fp * $self->{C},1);
	}

our $BigFloatOneHalf = Math::BigFloat->new('0.5');
our $BigFloatTen     = Math::BigFloat->new('10');
sub bigsqrt {
	#because the sqrt and root functions in Math::BigFloat sometimes fail
	#Wikipedia says:
	#sqrt(x) = 10**(1/2 * log_10(x))
	return $BigFloatTen->copy()->bpow($BigFloatOneHalf->copy()->bmul($_[0]->copy()->blog(10)),25);
	}

sub dimensionalStepFromTheta {

    # This is going to do the wrong thing sometimes for really curvy or loopy beziers.
    # But it's fine for some beziers, for some applications, and I think I make
    # good use of it.

    # AND we're currently doing linear distance between points on curve,
    # instead of distance along curve, and it could be that what I'm using it
    # for depends on that? Sometimes?

    # If you really want a dimensional step _along the curve_
    # you should probably make a seperate function for that.
    # (and keep this one, and figure what the two functions should be called)
    # Could be some of your new work on Bezier math in MPath could lead to a
    # decent approach for that. (You have offset curves there, and bez-bez intersections, can you pull off a true distance function?)

	my $self=shift;

	my $dim=shift;
	my $theta=shift;
	my $direction=scalar(@_)?shift:1; # 1 or 0

	my $findnexttheta = sub {
		my $ret;
		my $pt_last = $self->point($theta); # recalculated every time? Kind of thinking I did this to avoid some tricky bug? Leave alone unless you're ready to think through and test for problems if it's done once outside of this sub.
        if (ref($pt_last->[0])) {$pt_last->[0]=0 + sprintf("%.20f",$pt_last->[0]->bstr);}
        if (ref($pt_last->[1])) {$pt_last->[1]=0 + sprintf("%.20f",$pt_last->[1]->bstr);}
		if (!ref($_[0])) {
			#warn "\nNORMALLLLLLLLLLLLLLLLLLLLLLLLLLLLLL	\n";
			my $pt_new  = $self->point($_[0]);
            if (ref($pt_new->[0])) {$pt_new->[0]=0 + sprintf("%.20f",$pt_new->[0]->bstr);}
            if (ref($pt_new->[1])) {$pt_new->[1]=0 + sprintf("%.20f",$pt_new->[1]->bstr);}

			$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2);
			#print "$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2) = $ret\n";
			}
		else {
			warn "I don't think you want to be here - not sure if this mess is debugged.\n";
			my $pt_new  = $self->point($_[0]);
			print "using BigFloat with trial theta = $_[0]\n";
		#	my $dx=(ref($pt_new->[0]))?$pt_new->[0]->copy()->bsub($pt_last->[0]):$pt_new->[0] - $pt_last->[0];
		#	my $dxsqrd=(ref($dx))?$dx->copy()->bpow(2):$dx**2;
		#	my $dy=(ref($pt_new->[1]))?$pt_new->[1]->copy()->bsub($pt_last->[1]):$pt_new->[1] - $pt_last->[1];
		#	my $dysqrd=(ref($dy))?$dy->copy()->bpow(2):$dy**2;
		#	my $distsqrd=(ref($dysqrd))?$dysqrd->copy()->badd($dxsqrd):$dysqrd + $dxsqrd;
		#	my $dist = (ref($distsqrd))?bigsqrt($distsqrd):sqrt($distsqrd);

			my $dxsqrd = ref($pt_new->[0]) ? $pt_new->[0]->copy() : $pt_new->[0];
			if (ref($dxsqrd)) {$dxsqrd->bsub($pt_last->[0]);} else {$dxsqrd -= $pt_last->[0];}
			if (ref($dxsqrd)) {$dxsqrd->bpow(2);} else {$dxsqrd=$dxsqrd**2;}
			my $dysqrd = ref($pt_new->[1]) ? $pt_new->[1]->copy() : $pt_new->[1];
			if (ref($dysqrd)) {$dysqrd->bsub($pt_last->[1]);} else {$dysqrd -= $pt_last->[1];}
			if (ref($dysqrd)) {$dysqrd->bpow(2);} else {$dysqrd=$dysqrd**2;}
			my $distsqrd = ref($dysqrd) ? $dysqrd->copy() : $dysqrd;
			if (ref($distsqrd)) {$distsqrd->badd($dxsqrd)} else {$distsqrd += $dxsqrd;}
			my $dist = (ref($distsqrd))?bigsqrt($distsqrd):sqrt($distsqrd);

			if (ref($dim)) {
				#$ret= $dim - $dist;
				$ret= (0 + sprintf("%.20f",$dim->bstr)) -  (ref($dist) ? (0 + sprintf("%.20f",$dist->bstr)) : $dist);
				}
			else {
				my $dimB = Math::BigFloat->new(''.$dim);
				$ret= $dimB - $dist;
				}
			}
		return $ret;
		};

	my $newtheta;
	my $er;
    #warn "$dim , $theta , $direction , $self->{precision}";
    if (!defined($theta)) {warn "theta undefined";}
	($newtheta,$er) = FalsePosition($findnexttheta,($direction ? [$theta,1]:[0,$theta]),$self->{precision},($direction ? ($theta + (1-$theta)/2):($theta/2)),'dimensionalStepFromTheta for Bezier segment');
    #print " \nAFTR ROOT FIND: $newtheta , REF?:",(ref($newtheta) ? 'yes':'no'),"\n";
	if (length($newtheta)>17) {$newtheta=sprintf("%.15f",$newtheta);} #FalsePosition will return number as string if it ended up with BigFloat and none of inputs were BigFloat - so eval() here to turn that big string into a perl-sized number
    #print " dim step result ($newtheta,$er)\n";
	if (defined($er)) {
        warn "dimstep er: $er";
		#probably just reached the end
		if (abs(&{$findnexttheta}(($direction ? 1:0))) < $dim) {
			warn "snapping to end, I think, in dimensionalStepFromTheta";
			$newtheta=($direction ? 1:0);
			}
		#otherwise the error might be real
		else {warn "error in dimensionalStepFromTheta???? $er"}
		}
	my $r=$self->point($newtheta);
	if (ref($r->[0])) {$r->[0]=0 + sprintf("%.20",$r->[0]->bstr);}
	if (ref($r->[1])) {$r->[1]=0 + sprintf("%.20",$r->[1]->bstr);}
	return ($r,$newtheta);
	}

sub asin {
	#Based on Wolfram MathWorld
	#http://mathworld.wolfram.com/InverseSine.html
	#but it's supposed to use complex numbers, hmmm
	if    ($_[0] eq -1) {return $pi/2 * -1} #eq 2
	elsif ($_[0] eq  1) {return $pi/2     } #eq 4
	elsif ($_[0] eq  0) {return 0;        } #eq 3
	else {                                  #eq 15
# SPEED ATTEMPTS - why was this eval() here?
#		if (eval(abs($_[0]))>1) {warn("giving something bigger than +/-1 to your asin:",$_[0],"\n");}
		if (abs($_[0])>1) {warn("giving something bigger than +/-1 to your asin:",$_[0],"\n");}
		return atan2($_[0],sqrt(1-$_[0]**2));
		}
	}
sub acos {return ($pi/2) - asin($_[0]);}

}

####################################################################################
###      Math::MikePath::MoveTo        #############################################
package Math::MikePath::MoveTo;
{
push @Math::MikePath::MoveTo::ISA, 'Math::MikePath::LineSegment';

sub getLength {return 0;} #//per SVG spec
sub getFeet {return ();} #//per SVG spec
sub getIntersections {return ();} #//per SVG spec

}
####################################################################################
###      Math::MikePath::ClosePath     #############################################
package Math::MikePath::ClosePath;
{
push @Math::MikePath::ClosePath::ISA, 'Math::MikePath::LineSegment';
}
####################################################################################
###      Math::MikePath::LineSegment   #############################################
package Math::MikePath::LineSegment;
{
use Math::BigFloat;
use Math::Function::Root qw(FalsePosition); #get rid of this when/if you rewrite the dimensionalStepFromTheta sub
use Carp qw(cluck);


sub new {
	my $class = shift;
	my $self={};
	bless $self,$class;
	$self->{p1} = shift;
	$self->{p2} = shift;
	$self->{precision} = shift;
	$self->{isLite} = @_?shift:0;
	($self->{minx},$self->{maxx}) = sort {$a<=>$b} ($self->{p1}->[0],$self->{p2}->[0]);
	($self->{miny},$self->{maxy}) = sort {$a<=>$b} ($self->{p1}->[1],$self->{p2}->[1]);
	$self->{maxdiaglength} = sqrt(($self->{maxx} - $self->{minx})**2 + ($self->{maxy} - $self->{miny})**2);
	#$self->{thetaprecision} = $self->{precision}/$self->{maxdiaglength};
	$self->{m}  = ($self->{p2}->[0] - $self->{p1}->[0] == 0)?(($self->{p2}->[1] < $self->{p1}->[1])?'-':'').'inf':($self->{p2}->[1] - $self->{p1}->[1])/($self->{p2}->[0] - $self->{p1}->[0]);
	$self->{b}  = $self->{p1}->[1] - $self->{m} * $self->{p1}->[0];
	$self->{slopeTangent}=$self->{m};
	$self->{slopeNormal}= ($self->{slopeTangent} == 0)? (($self->{p2}->[0]<$self->{p1}->[0])?'-':'').'inf':-1/$self->{slopeTangent};
	$self->{dx}=$self->{p2}->[0] - $self->{p1}->[0];
	$self->{dy}=$self->{p2}->[1] - $self->{p1}->[1];
	$self->{angleTangent}=atan2($self->{dy},$self->{dx});
	$self->{angleNormal}=atan2(-$self->{dx},$self->{dy});
	$self->{length} = sqrt(($self->{p2}->[0] - $self->{p1}->[0])**2 + ($self->{p2}->[1] - $self->{p1}->[1])**2);
	$self->{length_big} = undef; # BigFloat used in point(theta) if theta is a ref. Create there only if needed.
	return $self;
	}

sub getInfiniteSlopeThetas {
	my $self = shift;
	if ($self->{dx}==0) {return wantarray ? (0,1):0;}
	else {return;}
	}

sub getLength {
	my $self = shift;
	my $res=shift;
	my $start_theta = shift;
	my $end_theta = shift;
	if (!defined($res)) {$res=1000;}
	if (!defined($start_theta)) {$start_theta=0;}
	if (!defined($end_theta)) {$end_theta=1;}
	return $self->{length} * abs($end_theta - $start_theta);
	}
sub precision {
	my $self=shift;
	if (defined($_[0])) {
		$self->{precision}=$_[0];
		#$self->{thetaprecision} = $self->{precision}/$self->{maxcoordval};
		}
	return $self->{precision};
	}
sub getRange {
	my $self = shift;
	return ($self->{minx},$self->{miny},$self->{maxx},$self->{maxy});
	}
sub inRange {
	my $self = shift;
	my $coords = shift;
	my $xok=0;
	my $yok=0;
	# eval()s seem to be needed for the == part of the <= test
	if (
	      defined($coords->[0]) &&
	      (
		     #eval($self->{minx}) < eval($coords->[0]) ||
		     $self->{minx} < $coords->[0] ||
		     #eval($self->{minx}) eq eval($coords->[0])
		     $self->{minx} eq $coords->[0]
		  ) &&
		  (
		     #eval($self->{maxx}) > eval($coords->[0]) ||
		     $self->{maxx} > $coords->[0] ||
		     #eval($self->{maxx}) eq eval($coords->[0])
		     $self->{maxx} eq $coords->[0]
		  )
		) {$xok=1;}
	if (
	      defined($coords->[1]) &&
		  (
		     #eval($self->{miny}) < eval($coords->[1]) ||
			 $self->{miny} < $coords->[1] ||
			 #eval($self->{miny}) eq eval($coords->[1])
		     $self->{miny} eq $coords->[1]
		  ) &&
		  (
		     #eval($self->{maxy}) > eval($coords->[1]) ||
			 $self->{maxy} > $coords->[1] ||
			 #eval($self->{maxy}) eq eval($coords->[1])
		     $self->{maxy} eq $coords->[1]
		  )
		) {$yok=1;}
	return $xok,$yok;
	}
sub onSegment {
	my $self = shift;
	my $point= shift;
	if ($point->[1] - ($self->{m} * $point->[0] + $self->{p1}->[0]) < $self->{precision}) {return 1;}
	else {return 0;}
	}
sub getFeet {
	my $self = shift;
	my $x=shift;
	my $y=shift;
	my @feet=();
	if ($self->{m} eq 0) {
		if (($self->inRange([$x,undef]))[0]) { push(@feet, [$x,$self->{p1}->[1], $self->solveXforTheta($x)] );}
		}
	elsif ($self->{m} =~ /inf/) {
		if (($self->inRange([undef,$y]))[1]) { push(@feet, [$self->{p1}->[0],$y, $self->solveYforTheta($y)] );}
		}
	else {
		my $intersect_x = (($self->{m}*$self->{p1}->[0])-($self->{p1}->[1])+((1/$self->{m})*$x)+($y))/($self->{m} + (1/$self->{m}));
		my $bx;
		my $by;
		($bx,$by) = $self->inRange([$intersect_x,undef]);
		if ($bx) {
			my $intersect_y = $self->f($intersect_x);
			($bx,$by) = $self->inRange([undef,$intersect_y]);
			if ($by) {
				#print " foot on line: [$intersect_x,$intersect_y]\n";
				#print "    for        [$x,$y]\n";
				push(@feet,[$intersect_x,$intersect_y, $self->solveXforTheta($intersect_x)]);
				}
			#else {print " foot canditate not in by range: [$x,$y]\n";}
			}
		#else {print " foot candidate not in bx range: [$x,$y]\n";}
		}
	return @feet;
	}
sub f {
	my $self = shift;
	my $x = shift;
	if ($self->{minx} > $x || $self->{maxx} < $x) {return;}
	elsif ($self->{m}=~/(\-?)inf/) {
		if (!wantarray) {return $self->{maxy};} #fixes my immediate problem, maybe, but what's the best policy for this whole inf slope thing?
		my $n = (0 + ($1.'1'));
		#this gives too many, and it's hard to figure a good general purpose way to come up with a number here
		#my $stepcount = abs(($self->{p2}->[1] - $self->{p1}->[1])/($self->{precision} * 10));
		#print "stepcount: $stepcount = abs(($self->{p2}->[1] - $self->{p1}->[1])/($self->{precision} * 10));\n";
		#so lets say 20 for now and revisit next time we're stuck on this situation
		my $stepcount=20;
		my $wholestepcount=int($stepcount);
		my $remainder = $stepcount - $wholestepcount;
		my $step = $n * $self->{precision};
		my @ret=();
		foreach (0 .. $wholestepcount) {push(@ret,$self->{p1}->[1] + $step * $_);}
		push(@ret,$self->{p1}->[1] + $step * $wholestepcount + $step * $remainder);
		#cluck "made multiple ret vals for f() for line segment [[$self->{p1}->[0],$self->{p1}->[1]],[$self->{p2}->[0],$self->{p2}->[1]]] because m = $self->{m}  (which should be '+/-inf')\n";
		#print "made multiple ret vals for f() for line segment [[$self->{p1}->[0],$self->{p1}->[1]],[$self->{p2}->[0],$self->{p2}->[1]]] because m = $self->{m}  (which should be '+/-inf')\n";
		return @ret;
		}
	elsif ($self->{m} eq 0) {return $self->{p1}->[1];}
	elsif ($x eq $self->{p1}->[0]) {return $self->{p1}->[1];}
	elsif ($x eq $self->{p2}->[0]) {return $self->{p2}->[1];}
	else {return $self->{m} * $x + $self->{b};}
	}
sub F {
	my $self = shift;
	my $y = shift;
	if ($self->{miny} > $y || $self->{maxy} < $y) {return;}
	if ($self->{m} eq 0) {
		if (!wantarray) {return $self->{maxx};} #fixes my immediate problem, but what's the best poicy for this whole inf slope thing?
		my $n = ($self->{p2}->[0] > $self->{p1}->[0])?1:-1;
		#my $stepcount = abs(($self->{p2}->[0] - $self->{p1}->[0])/$self->{precision});
		my $stepcount = abs(($self->{p2}->[0] - $self->{p1}->[0])/10); #hmm
		my $wholestepcount=int($stepcount);
		my $remainder = $stepcount - $wholestepcount;
		my $step = $n * $self->{precision};
		my @ret=();
		foreach (0 .. $wholestepcount) {push(@ret,$self->{p1}->[0] + $step * $_);}
		push(@ret,$self->{p1}->[0] + $step * $wholestepcount + $step * $remainder);
		print "making multiple ret vals for F() for line segment because m = $self->{m}  (which should be '0')\n";
		return @ret;
		}
	elsif ($self->{m}=~/(\-?)inf/) {return $self->{p1}->[0];}
	else {return ($y - $self->{b})/$self->{m};}
	}
sub solveXforTheta {
	my $self=shift;
	my $x=shift;
	my $th = abs(($x - $self->{p1}->[0])/($self->{p2}->[0] - $self->{p1}->[0]));
    if ((1 > $th || 1 eq $th) && ($th > 0 || $th eq 0)) {return ($th);}
    else {return ();}
	}
sub solveYforTheta {
	my $self=shift;
	my $y=shift;
	if ($self->{p2}->[1] - $self->{p1}->[1] eq 0) {warn "[$self->{p1}->[0] , $self->{p1}->[1]]\n[$self->{p2}->[0] , $self->{p2}->[1]]\n"}
	my $th = abs(($y - $self->{p1}->[1])/($self->{p2}->[1] - $self->{p1}->[1]));
    if ((1 > $th || 1 eq $th) && ($th > 0 || $th eq 0)) {return ($th);}
    else {return ();}
	}
sub point {
	my $self=shift;
	my $theta=shift;
	my ($x,$y);
	if ($theta==0)    { $x=$self->{p1}->[0];$y=$self->{p1}->[1];}
	elsif ($theta==1) { $x=$self->{p2}->[0];$y=$self->{p2}->[1];}
	else {
		# At some point I started doing all line segment point lookups
		# with BigFloats. It didn't hurt too much at the time, and it
		# allowed the 0-to-1 paramaterization to work with longer
		# lines when doing fine-grained root finding.
		# But later I was processing lots of short lines
		# and this became a bottleneck. So, back to normal PERL-sized
		# numbers.
		#
		# What's needed is a good way to choose when to use Bigs.
		#
		# For now I'll say, if you pass in a BigFloat theta, you'll
		# get BigFloat processing and a BigFloat response.
		#
		if (!ref($theta)) { # if theta is not a reference to a BigFloat, just do the normal simple math with normal numbers
			$x=($self->{dx} * $theta) + $self->{p1}->[0];
			$y=($self->{dy} * $theta) + $self->{p1}->[1];
#			print "old[$x,$y]\n";
			}
		else {
            if (!defined $self->{length_big}) {$self->{length_big} = new Math::BigFloat ''.$self->{length};}
			my $partdist=$self->{length_big}->copy()->bmul($theta);
			$x=$partdist->copy()->bmul(CORE::cos($self->{angleTangent}));
			$y=$partdist->copy()->bmul(CORE::sin($self->{angleTangent}));
			#print "lpa: $x + $self->{p1}->[0] = ";
			$x->badd($self->{p1}->[0]);
			#print "$x\n";
			$y->badd($self->{p1}->[1]);
			if (!ref($theta)) {
# SPEED ATTEMPTS
#				if (ref($x)) {$x=eval(substr($x->bstr(),0,18));}
#				if (ref($y)) {$y=eval(substr($y->bstr(),0,18));}
				if (ref($x)) {$x=0 + sprintf("%.20f",$x->bstr());}
				if (ref($y)) {$y=0 + sprintf("%.20f",$y->bstr());}
				}
			#print "new[$x,$y]\n";
			}
		}

	#print "POINT FROM LINE: ($x,$y)   [$self->{p1}->[0],$self->{p1}->[1]],[$self->{p2}->[0],$self->{p2}->[1]]\n";
	return [$x,$y];
	}
sub secondDerivative {return 0;}
sub slopeTangent {return $_[0]->{slopeTangent};}
sub slopeTangent_byTheta {return $_[0]->{slopeTangent};}
sub fDoublePrime {return 0};
sub slopeNormal  {return $_[0]->{slopeNormal};}
sub slopeNormal_byTheta  {return $_[0]->{slopeNormal};}
sub angleTangent {return $_[0]->{angleTangent};}
sub angleNormal  {return $_[0]->{angleNormal};}
sub angleTangent_byTheta {return $_[0]->{angleTangent};}
sub angleNormal_byTheta  {return $_[0]->{angleNormal};}

our $BigFloatOneHalf = Math::BigFloat->new('0.5');
our $BigFloatTen     = Math::BigFloat->new('10');
sub bigsqrt {
	#because the sqrt and root functions in Math::BigFloat sometimes fail
	#Wikipedia says:
	#sqrt(x) = 10**(1/2 * log_10(x))
	return $BigFloatTen->copy()->bpow($BigFloatOneHalf->copy()->bmul($_[0]->copy()->blog(10)),25);
	}

sub dimensionalStepFromTheta {


	# Oh, things don't run as fast as you would like?
	# Well, why don't you rewrite this for the simple line segment case, instead of using
	# the copy-pasted code from CubicBezier - duh.
	# (actually this is pretty fast - not likely a bottleneck - still, rewrite it sometime)
	# and it looks like I did this first for all the javascript versions
	# including arc, so if it's good enough for javascript, maybe it's
	# good enough for PERL


	my $self=shift;

	my $dim=shift;
	my $theta=shift;
	my $direction=scalar(@_)?shift:1;

	my $pt_last = $self->point($theta);

	my $findnexttheta = sub {
		my $ret;
		#my $pt_last = $self->point($theta);
		if (!ref($_[0])) {
			my $pt_new  = $self->point($_[0]);
			$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2);
			#print "$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2) = $ret\n";
			}
		else {
			#warn "I don't think you want to be here - not sure if this mess is debugged.\n";
			my $pt_new  = $self->point($_[0]);
			#print "using BigFloat - \n";
			my $dx=(ref($pt_new->[0]))?$pt_new->[0]->copy()->bsub($pt_last->[0]):$pt_new->[0] - $pt_last->[0];
			my $dxsqrd=(ref($dx))?$dx->bpow(2):$dx**2;
			my $dy=(ref($pt_new->[1]))?$pt_new->[1]->copy()->bsub($pt_last->[1]):$pt_new->[1] - $pt_last->[1];
			my $dysqrd=(ref($dy))?$dy->bpow(2):$dy**2;
			my $distsqrd=(ref($dysqrd))?$dysqrd->copy()->badd($dxsqrd):$dysqrd + $dxsqrd;
			my $dist = (ref($distsqrd))?bigsqrt($distsqrd):sqrt($distsqrd);
			$ret = (ref($dim) ? $dim:Math::BigFloat->new(''.$dim)) - $dist;
			}
		return $ret;
		};

	my $newtheta;
	my $er;
    #warn "$dim , $theta , $direction , $self->{precision}";
	($newtheta,$er) = FalsePosition($findnexttheta,($direction ? [$theta,1]:[0,$theta]),$self->{precision},($theta+($direction ? 1:0))/2,'dimensionalStepFromTheta for Line segment');
	#print " dim step result ($newtheta,$er)\n";
	if (defined($er)) {
        warn "dimstep er: $er";
		#probably just reached the end
		if (abs(&{$findnexttheta}(($direction ? 1:0))) < $dim) {
			$newtheta=($direction ? 1:0);
			}
		#otherwise the error might be real
		}
	return ($self->point($newtheta),$newtheta);
	}
}
####################################################################################
###      Math::MikePath::EllipticalArc           ###################################
# http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes
package Math::MikePath::EllipticalArc;
{
use Math::QuadraticFormula;
use Math::Function::Root qw(BrentsMethod FalsePosition);
our $pi = 4 * atan2(1,1);

sub new {
	my $class = shift;
	my $self={};
	bless $self,$class;
	$self->{p1} = shift;
	$self->{r} = shift;
	$self->{rx} = abs($self->{r}->[0]);
	$self->{ry} = abs($self->{r}->[1]);
	$self->{phi} = shift;
	$self->{phi} = ((1000000*$self->{phi}) % (360000000))/1000000; # angle starts as degrees, mod 360
	$self->{phi_radians} = $self->{phi} * ($pi/180);
	$self->{large_arc_flag} = shift;
	$self->{sweep_flag} = shift;
	if ($self->{large_arc_flag} ne 0) {$self->{large_arc_flag}=1;}
	if ($self->{sweep_flag} ne 0) {$self->{sweep_flag}=1;}
	$self->{p2} = shift;
	#warn "eh: [$self->{p1}->[0],$self->{p1}->[1]] , [$self->{rx},$self->{rx}] , $self->{phi} , $self->{large_arc_flag} , $self->{sweep_flag} , [$self->{p2}->[0],$self->{p2}->[1]] \n";
	$self->{precision}=shift;
	$self->{isLite}=shift;
	$self->{maxdiaglength} = sqrt(($self->{maxx} - $self->{minx})**2 + ($self->{maxy} - $self->{miny})**2);

	#calculate the center of the ellipse
	#step 1: Compute (x1', y1')
	my $x1prime=(($self->{p1}->[0] - $self->{p2}->[0])/2) *  cos($self->{phi_radians})  +  (($self->{p1}->[1] - $self->{p2}->[1])/2) * sin($self->{phi_radians});
	my $y1prime=(($self->{p1}->[0] - $self->{p2}->[0])/2) * -sin($self->{phi_radians})  +  (($self->{p1}->[1] - $self->{p2}->[1])/2) * cos($self->{phi_radians});
	#make sure they are large enough to make an ellipse that will reach the destination point
    if    ($self->{rx}==0 || $self->{rx}==0) {warn "elliptical arc with zero x or y radius. x,y:",$self->{p2}->[0],", ",$self->{p2}->[1],", rx,ry: $self->{rx}, $self->{ry}\n";}
    # avoid divide by zero and force $lam to be too big if rx or ry == 0
    # ... but then you're just kicking the problem down to later divide by zero cases.
    # TODO: the issue is, what do you do with zero-radius arcs? let them exist? If so, should probably skip over all this attempt to calc with 0.
    #       what about zero length line segs, zero length beziers?
    #       But in those cases and this, the endpoints should give that away. Like, zero radius must be secondary to p1 and p2 being the same.
    #       Diff here would be with possible full arc with largearcflag=1.
    #       Hmm.
	my $lam = ($self->{rx} == 0 || $self->{ry} == 0 ) ? 2 : ($x1prime)**2/($self->{rx}**2) + ($y1prime)**2/($self->{ry}**2);
	if ($lam > 1) {
		my $sqrtlam = sqrt($lam);
		$self->{rx}*=$sqrtlam+0.000000001;
		$self->{ry}*=$sqrtlam+0.000000001;
		}
	#step 2: Compute (cX ', cY  ')


    my $to_sqrt_for_hairy_radical;
    if ($x1prime == 0 && $y1prime == 0) {$to_sqrt_for_hairy_radical=0;} # otherwise we get divide by zero
    else {
        $to_sqrt_for_hairy_radical = ( ($self->{rx}**2 * $self->{ry}**2) - ($self->{rx}**2 * $y1prime**2) - ($self->{ry}**2 * $x1prime**2) )/( ($self->{rx}**2 * $y1prime**2) + ($self->{ry}**2 * $x1prime**2) );
        }

	if ($to_sqrt_for_hairy_radical<0 && abs($to_sqrt_for_hairy_radical)<10**-15) {$to_sqrt_for_hairy_radical=0;} #had some really small negative - like 10**-19 - choking sqrt. Maybe should find how that happened. But snapping to zero for now to get work done.
	#my $hairy_radical = (($self->{large_arc_flag} eq $self->{sweep_flag})?-1:1) * sqrt(( ($self->{rx}**2 * $self->{ry}**2) - ($self->{rx}**2 * $y1prime**2) - ($self->{ry}**2 * $x1prime**2) )/( ($self->{rx}**2 * $y1prime**2) + ($self->{ry}**2 * $x1prime**2) ));
	my $hairy_radical = (($self->{large_arc_flag} eq $self->{sweep_flag})?-1:1) * sqrt($to_sqrt_for_hairy_radical);
	my $cxprime = $hairy_radical *      (($self->{rx} * $y1prime)/$self->{ry});
	my $cyprime = $hairy_radical * -1 * (($self->{ry} * $x1prime)/$self->{rx});
	#step 3: Compute (cX, cY) from (cX ', cY  ')
	$self->{cx} = $cxprime * cos($self->{phi_radians}) + $cyprime * -sin($self->{phi_radians}) + ($self->{p1}->[0] + $self->{p2}->[0])/2;
	$self->{cy} = $cxprime * sin($self->{phi_radians}) + $cyprime *  cos($self->{phi_radians}) + ($self->{p1}->[1] + $self->{p2}->[1])/2;
	#Step 4: Compute theta1 	and 	delta-theta
	#my $theta1_arccos_arg = (($y1prime - $cyprime)/$self->{ry})/sqrt((($y1prime - $cyprime)/$self->{ry})**2 + (($x1prime - $cxprime)/$self->{rx})**2);
    my $theta1_arccos_arg;
    if ($x1prime ==0 && $cxprime==0) {$theta1_arccos_arg =0;}
    else {
    	$theta1_arccos_arg =  (($x1prime - $cxprime)/$self->{rx})/sqrt((($x1prime - $cxprime)/$self->{rx})**2 + (($y1prime - $cyprime)/$self->{ry})**2);
    }
	my $theta1sign=(($y1prime-$cyprime)/$self->{ry}) < 0 ? -1:1; # - ((0)*(...))   ux*vy-uy*vx;
	$self->{theta1} = $theta1sign * ($pi/2 - asin($theta1_arccos_arg));

    my $delta_theta_arccos_arg;

    if ( $x1prime == 0 && $cxprime == 0 && $y1prime == 0 && $cyprime == 0) {$delta_theta_arccos_arg =0;}
    else {
    	$delta_theta_arccos_arg =  ((($x1prime - $cxprime)/$self->{rx}) * ((-$x1prime - $cxprime)/$self->{rx}) + (($y1prime - $cyprime)/$self->{ry}) * ((-$y1prime - $cyprime)/$self->{ry}))/sqrt((($x1prime - $cxprime)/$self->{rx})**2 + (($y1prime - $cyprime)/$self->{ry})**2);
        }

	my $delta_theta_sign=(
		( (($x1prime - $cxprime)/$self->{rx}) * ((-$y1prime - $cyprime)/$self->{ry}) )
			-
		( (($y1prime - $cyprime)/$self->{ry}) * ((-$x1prime - $cxprime)/$self->{rx}) )

		< 0)?-1:1;

	#print "TSign: $theta1sign = (($y1prime-$cyprime)/$self->{ry})) < 0\nDsign: $delta_theta_sign\n";
	#$self->{delta_theta} = $delta_theta_sign * (($pi/2) - atan2($delta_theta_arccos_arg,sqrt(1-$delta_theta_arccos_arg**2)));
	$self->{delta_theta} = $delta_theta_sign * (($pi/2) - asin($delta_theta_arccos_arg));



	#Make all these theta mods rational, in accordance with what's supposed to happen, overcoming any angle flattenning that comes from atan sqrt usage above
	#print "LA: $self->{large_arc_flag} , SW: $self->{sweep_flag}\n";
	#print "PREMOD:  t1,t2,dt:$self->{theta1},",($self->{theta1} + $self->{delta_theta}),",$self->{delta_theta}\n";
	#mod(360deg) for delta_theta
	if (abs($self->{delta_theta}) > 2*$pi) {
		my $div=$self->{delta_theta}/(2*$pi);
		my $rem=$div - int($div);
		print "DIVVING ($div - ",int($div),") = ",$rem,"\n";
		$self->{delta_theta} = $rem * (2*$pi);
		}

	if ($self->{sweep_flag}) {
		if ($self->{delta_theta} < 0) {
			if (!$self->{large_arc_flag}) {$self->{delta_theta} *= -1 ;}
			else {$self->{delta_theta} += 2 * $pi;}
			}
		}
	else {
		if ($self->{delta_theta} > 0) {$self->{delta_theta} -= 2 * $pi;}
		if ($self->{large_arc_flag} && $self->{delta_theta} > -$pi) {$self->{delta_theta} = -(2*$pi+$self->{delta_theta});}
		}

	$self->{theta2}=$self->{theta1} + $self->{delta_theta};
	#print "POSTMOD: t1,t2,dt:$self->{theta1},$self->{theta2},$self->{delta_theta}\n";



	#calculate the foci of the ellipse
	$self->{f1}= $self->{rx} > $self->{ry} ? [sqrt($self->{rx}**2 - $self->{ry}**2),0] : [0,sqrt($self->{ry}**2 - $self->{rx}**2)];
	$self->{f2}= $self->{rx} > $self->{ry} ? [-$self->{f1}->[0],$self->{f1}->[1]] : [$self->{f1}->[0],-$self->{f1}->[1]];
	#now is a good time to calculate eccentricity, too - used in circumference and arc calculations
	$self->{eccentricity}=$self->{f1}/(($self->{rx}>$self->{ry})?$self->{rx}:$self->{ry});
	$self->{f1} = _rotate2d([0,0],$self->{f1},$self->{phi_radians});
	$self->{f1}->[0]+=$self->{cx};
	$self->{f1}->[1]+=$self->{cy};
	$self->{f2} = _rotate2d([0,0],$self->{f2},$self->{phi_radians});
	$self->{f2}->[0]+=$self->{cx};
	$self->{f2}->[1]+=$self->{cy};

	if (!$self->{isLite}) {
		$self->initBigs();
		}

	my @extremexs_is=(0,((!$self->{isLite})?$self->solveXPrimeforThetaBig(Math::BigFloat->bzero()):$self->solveXPrimeforTheta(0)),1);
	my @extremexs = map {(ref($_) && !$self->{isLite}) ? $self->evalXofThetaBig($_):$self->evalXofTheta($_)} @extremexs_is;
	my @extremexs_sorted = sort {$a<=>$b} @extremexs;

	my @extremeys_is=(0,((!$self->{isLite})?$self->solveYPrimeforThetaBig(Math::BigFloat->bzero()):$self->solveYPrimeforTheta(0)),1);
	my @extremeys = map {(ref($_) && !$self->{isLite}) ? $self->evalYofThetaBig($_):$self->evalYofTheta($_)} @extremeys_is;
	my @extremeys_sorted = sort {$a<=>$b} @extremeys;

	$self->{extremexs_is}=[(@extremexs_is)];
	$self->{extremeys_is}=[(@extremeys_is)];
	$self->{extremexs}=[(@extremexs)];
	$self->{extremeys}=[(@extremeys)];
	$self->{minx} = $extremexs_sorted[0];
	$self->{maxx} = $extremexs_sorted[$#extremexs];
	$self->{miny} = $extremeys_sorted[0];
	$self->{maxy} = $extremeys_sorted[$#extremeys];


	if (!$self->{isLite}) {
		$self->initDangerRanges();
		}

#	my %dupsieve_x;
#	my @extremexs_is=grep {!$dupsieve_x{$_}++} ($self->{theta1},($self->solveXPrimeforTheta(0)),$self->{theta2});
#	#print "EXTREME X Is: ",join(",",@extremexs_is),"\n";
#	my @extremexs = map {$self->evalXofTheta($_)} @extremexs_is;
#	my @extremexs_sorted = sort {$a<=>$b} @extremexs;
#	my %dupsieve_y;
#	my @extremeys_is=grep {!$dupsieve_y{$_}++} ($self->{theta1},($self->solveYPrimeforTheta(0)),$self->{theta2});
#	#print "EXTREME Y Is: ",join(",",@extremeys_is),"\n";
#	my @extremeys = map {$self->evalYofTheta($_)}  @extremeys_is;
#	my @extremeys_sorted = sort {$a<=>$b} @extremeys;
#	$self->{extremexs_is}=[(@extremexs_is)];
#	$self->{extremeys_is}=[(@extremeys_is)];
#	$self->{extremexs}=[(@extremexs)];
#	$self->{extremeys}=[(@extremeys)];
#	$self->{minx} = $extremexs_sorted[0];
#	$self->{maxx} = $extremexs_sorted[$#extremexs];
#	$self->{miny} = $extremeys_sorted[0];
#	$self->{maxy} = $extremeys_sorted[$#extremeys];

#	#adapted from cubic bezier code:
#	my $perlprecision = Math::BigFloat->new('0.00000000000001');
#	$self->{maxdiaglength}=~/([0-9]+)\./;
#	$self->{curveprecision}=$perlprecision * Math::BigFloat->new(10**(length(($1*$1)))) ;
##	print "curve precision: ",$self->{curveprecision},"\n";
#	my @xdangeris = sort {$a<=>$b} ( grep {defined} ( $self->solveXPrimeforTheta($self->{curveprecision})  , $self->solveXPrimeforTheta(-$self->{curveprecision})));
#	my @ydangeris = sort {$a<=>$b} ( grep {defined} ( $self->solveYPrimeforTheta($self->{curveprecision})  , $self->solveYPrimeforTheta(-$self->{curveprecision})));
#
#	my @mxidangerranges;
#	for (my $i=0;$i<@xdangeris;$i++) {
#		my $p=$self->evalXPrimeofTheta($xdangeris[$i]);
#		my $pp=$self->evalXDoublePrimeofTheta($xdangeris[$i]);
#		if (($p < 0 && $pp < 0) || (($p > 0 || $p eq 0) && ($pp > 0 || $pp eq 0))) {
#			#is end of range
#			if ($i == 0)          {
#				push(@mxidangerranges,[(sort {$a<=>$b} ($self->{theta1},$self->{theta2}))[0],$xdangeris[$i]]);
#				}
#			else {
#				push(@mxidangerranges,[$xdangeris[$i-1],$xdangeris[$i]]);
#				}
#			}
#		elsif ($i == $#xdangeris) {push(@mxidangerranges,[$xdangeris[$i],(sort {$b<=>$a} ($self->{theta1},$self->{theta2}))[0]]);}
#		}
	$self->{Fydangerranges} = [];
#	$self->{Fydangerranges}->[scalar(@{$self->{Fydangerranges}})]=[$self->{p1}->[1],$self->{p1}->[1]];
#	$self->{Fydangerranges}->[scalar(@{$self->{Fydangerranges}})]=[$self->{p2}->[1],$self->{p2}->[1]]; #try to be more exact at endpoints
#	$self->{mxidangerranges} = \@mxidangerranges;
#	foreach (@mxidangerranges) {
#		my $y1=$self->evalYofTheta($_->[0]);
#		my $y2=$self->evalYofTheta($_->[1]);
#		if (abs($y2-$y1)>$self->{curveprecision}) {
#			$self->{Fydangerranges}->[scalar(@{$self->{Fydangerranges}})]=[eval(ref($y1)?$y1->bstr:$y1),eval(ref($y2)?$y2->bstr:$y2)];
#			}
#		}
##	print "danger xi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @mxidangerranges),' from xis: (',join(',',@xdangeris),')' ,"\n";
##	print "danger Fy ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @{$self->{Fydangerranges}}) , "\n";
#
#	my @myidangerranges;
	$self->{fxdangerranges} = [];
#	$self->{fxdangerranges}->[scalar(@{$self->{fxdangerranges}})]=[$self->{p1}->[0],$self->{p1}->[0]];
#	$self->{fxdangerranges}->[scalar(@{$self->{fxdangerranges}})]=[$self->{p2}->[0],$self->{p2}->[0]]; #try to be more exact at endpoints
#	for (my $i=0;$i<@ydangeris;$i++) {
#		my $p=$self->evalYPrimeofTheta($ydangeris[$i]);
#		my $pp=$self->evalYDoublePrimeofTheta($ydangeris[$i]);
#		if (($p < 0 && $pp < 0) || (($p > 0 || $p eq 0) && ($pp > 0 || $pp eq 0))) {
#			#is end of range
#			if ($i == 0)          {
#				push(@myidangerranges,[(sort {$a<=>$b} ($self->{theta1},$self->{theta2}))[0],$ydangeris[$i]]);
#				}
#			else {
#				push(@myidangerranges,[$ydangeris[$i-1],$ydangeris[$i]]);
#				}
#			}
#		elsif ($i == $#ydangeris) {push(@myidangerranges,[$ydangeris[$i],(sort {$b<=>$a} ($self->{theta1},$self->{theta2}))[0]]);}
#		}
#	$self->{myidangerranges} = \@myidangerranges;
#	foreach (@myidangerranges) {
#		my $x1=$self->evalXofTheta($_->[0]);
#		my $x2=$self->evalXofTheta($_->[1]);
#		if (abs($x2-$x1)>$self->{curveprecision}) {
#			$self->{fxdangerranges}->[scalar(@{$self->{fxdangerranges}})]=[eval($x1),eval($x2)];
##			print "dif is : ",abs($x2-$x1),"\n";
#			}
#		}
##	print "danger yi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @myidangerranges),' from yis: (',join(',',@ydangeris),')' ,"\n";
##	print "danger fx ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @{$self->{fxdangerranges}}) , "\n";


	$self->{length}=getLength($self,1000,0,1);

#print " ahahahhahahaha: $self->{cx}, $self->{cy}\n";

	return $self;
	}

sub solveXPrimeforThetaBig {return $_[0]->solveXPrimeforTheta($_[1])}
sub solveYPrimeforThetaBig {return $_[0]->solveYPrimeforTheta($_[1])}
sub evalXofThetaBig {return $_[0]->evalXofTheta($_[1])}
sub evalYofThetaBig {return $_[0]->evalYofTheta($_[1])}

sub initDangerRanges {
	my $self=shift;


#redo these in all big - this might muck stuff up, so might delete this section later
	my @extremexs_is=(0,($self->solveXPrimeforThetaBig(Math::BigFloat->bzero())),1);
	my @extremexs = map {ref($_)?$self->evalXofThetaBig($_):$self->evalXofTheta($_)} @extremexs_is;
	my @extremexs_sorted = sort {$a<=>$b} @extremexs;

	my @extremeys_is=(0,($self->solveYPrimeforThetaBig(Math::BigFloat->bzero())),1);
	my @extremeys = map {ref($_)?$self->evalYofThetaBig($_):$self->evalYofTheta($_)} @extremeys_is;
	my @extremeys_sorted = sort {$a<=>$b} @extremeys;
#print "[$self->{p1}->[0],$self->{p1}->[1]][$self->{p2}->[0],$self->{p2}->[1]][$self->{rx},$self->{ry}] BUT extremeys:",join(',',@extremeys)," FROM ",join(',',@extremeys_is),"\n";
	$self->{extremexs_is}=[(@extremexs_is)];
	$self->{extremeys_is}=[(@extremeys_is)];
	$self->{extremexs}=[(@extremexs)];
	$self->{extremeys}=[(@extremeys)];
	$self->{minx} = $extremexs_sorted[0];
	$self->{maxx} = $extremexs_sorted[$#extremexs];
	$self->{miny} = $extremeys_sorted[0];
	$self->{maxy} = $extremeys_sorted[$#extremeys];
#end redo



	#my $perlprecision = Math::BigFloat->new('0.00000000000001');
	my $perlprecision =                       0.00000000000001;
	my $maxdim=(sort {$b<=>$a} map {abs($_)} ($self->{maxx},$self->{maxy},$self->{minx},$self->{miny}))[0];
	$maxdim=~/^([0-9]+)\./;
	#$self->{curveprecision}=$perlprecision * Math::BigFloat->new(''.(10**(length($1)))) ;
	$self->{curveprecision}=$perlprecision * (10**(length($1))) ;
	#print "curve precision: ",$self->{curveprecision},"\n";
	#was doing ...PrimeforThetaBig() for all these, but that takes about 1.5 seconds rather than something like 0.0003
	my @xdangeris = sort {$a<=>$b} (
		($self->solveXPrimeforTheta($perlprecision/$self->{curveprecision}))  , # near zero slope, positive
	   #($self->solveXPrimeforTheta(Math::BigFloat->bzero())),                  # zero slope
		($self->solveXPrimeforTheta(0)),                  # zero slope
		($self->solveXPrimeforTheta(-$perlprecision/$self->{curveprecision})) , # near zero slope, negative
		($self->solveXPrimeforTheta($self->{curveprecision}/$perlprecision)) ,  # toward infinite slope, positive
		($self->solveXPrimeforTheta(-$self->{curveprecision}/$perlprecision)) , # toward infinite slope, negative
		);
	my @ydangeris = sort {$a<=>$b} (
		($self->solveYPrimeforTheta($perlprecision/$self->{curveprecision}))  ,
	   #($self->solveYPrimeforTheta(Math::BigFloat->bzero())) ,
		($self->solveYPrimeforTheta(0)) ,
		($self->solveYPrimeforTheta(-$perlprecision/$self->{curveprecision})) ,
		($self->solveYPrimeforTheta($self->{curveprecision}/$perlprecision)) ,
		($self->solveYPrimeforTheta(-$self->{curveprecision}/$perlprecision)) ,
		);

	my @mxidangerranges;
	for (my $i=0;$i<@xdangeris;$i++) {
		my $p=$self->evalXPrimeofTheta($xdangeris[$i]);
		my $pp=$self->evalXDoublePrimeofTheta($xdangeris[$i]);
		if (($p < 0 && $pp < 0) || (($p > 0 || $p eq 0) && ($pp > 0 || $pp eq 0))) {
			#is end of range
			if ($i eq 0)          {push(@mxidangerranges,[0,(sort {$a<=>$b} (1,($xdangeris[$i] + $self->{curveprecision})))[0]]);}
			else                  {push(@mxidangerranges,[(sort {$b<=>$a} (0,($xdangeris[$i-1] - $self->{curveprecision})))[0],(sort {$a<=>$b} (1,($xdangeris[$i] + $self->{curveprecision})))[0]]);}
			}
		elsif ($i eq $#xdangeris) {push(@mxidangerranges,[(sort {$b<=>$a} (0,($xdangeris[$i] - $self->{curveprecision})))[0],1]);}
		}
	$self->{mxidangerranges} = \@mxidangerranges;

	$self->{xofidangerranges} = [];
	$self->{xofidangerranges}->[scalar(@{$self->{xofidangerranges}})]=[$self->{p1}->[0],$self->{p1}->[0]]; #zero-length "range" to
	$self->{xofidangerranges}->[scalar(@{$self->{xofidangerranges}})]=[$self->{p2}->[0],$self->{p2}->[0]]; #try to be more exact at endpoints

	foreach (@mxidangerranges) {
		$self->{xofidangerranges}->[scalar(@{$self->{xofidangerranges}})]=[$self->evalXofTheta($_->[0]),$self->evalXofThetaBig($_->[1])];
		}
	#print "danger xi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @mxidangerranges),' from xis: (',join(',',@xdangeris),')' ,"\n";
	#print "danger xofi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @{$self->{xofidangerranges}}) , "\n";

	my @myidangerranges;
	$self->{yofidangerranges} = [];
	$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[$self->{p1}->[1],$self->{p1}->[1]];
	$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[$self->{p2}->[1],$self->{p2}->[1]]; #try to be more exact at endpoints
	for (my $i=0;$i<@ydangeris;$i++) {
		my $p=$self->evalYPrimeofTheta($ydangeris[$i]);
		my $pp=$self->evalYDoublePrimeofTheta($ydangeris[$i]);
		if (($p < 0 && $pp < 0) || (($p > 0 || $p eq 0) && ($pp > 0 || $pp eq 0))) {
			#is end of range
			if ($i == 0)          {push(@myidangerranges,[0,(sort {$a<=>$b} (1,$ydangeris[$i] + $self->{curveprecision}))[0]]);}
			else                  {push(@myidangerranges,[(sort {$b<=>$a} (0,$ydangeris[$i-1] - $self->{curveprecision}))[0],(sort {$a<=>$b} (1,$ydangeris[$i] + $self->{curveprecision}))[0]]);}
			}
		elsif ($i == $#ydangeris) {push(@myidangerranges,[(sort {$b<=>$a} (0,$ydangeris[$i] - $self->{curveprecision}))[0],1]);}
		}
	$self->{myidangerranges} = \@myidangerranges;
	foreach (@myidangerranges) {
		$self->{yofidangerranges}->[scalar(@{$self->{yofidangerranges}})]=[$self->evalYofThetaBig($_->[0]),$self->evalYofThetaBig($_->[1])];
		}
	#print "danger yi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @myidangerranges),' from yis: (',join(',',@ydangeris),')' ,"\n";
	#print "danger yofi ranges: ", join('  ',map {'['.$_->[0].','.$_->[1].']'} @{$self->{yofidangerranges}}) , "\n";
	}
sub initBigs {
	my $self=shift;
	#nothing here for arc, right?
	$self->{bigInitted}=1;
	}
sub precision {
	my $self=shift;
	if (defined($_[0])) {
		$self->{precision}=$_[0];
		#$self->{thetaprecision} = $self->{precision}/$self->{maxcoordval};
		}
	return $self->{precision};
	}
sub getRange {
	my $self = shift;
	return ($self->{minx},$self->{miny},$self->{maxx},$self->{maxy});
	}
sub inRange {
	my $self = shift;
	my $coords = shift;
	my $xok=0;
	my $yok=0;
	# should get rid of all these eval()s
	# but inRange is a potential source of bugginess, as experienced with the
	# bezier version, so don't mess unless you're ready to test
	if (defined($coords->[0]) && (eval($self->{minx}) < eval($coords->[0]) || eval($self->{minx}) eq eval($coords->[0])) && (eval($self->{maxx}) > eval($coords->[0]) || eval($self->{maxx}) eq eval($coords->[0]))) {$xok=1;}
	if (defined($coords->[1]) && (eval($self->{miny}) < eval($coords->[1]) || eval($self->{miny}) eq eval($coords->[1])) && (eval($self->{maxy}) > eval($coords->[1]) || eval($self->{maxy}) eq eval($coords->[1]))) {$yok=1;}
	return $xok,$yok;
	}
sub arcThetaToNormalizedTheta {
	my $self=shift;
	my $arcTheta=shift;
	my $num = ($arcTheta - $self->{theta1});
	return 0 if $num == 0; # avoids divide by zero error when below comes out as 0/0
	return $num/$self->{delta_theta};
	}
sub normalizedThetaToArcTheta {
	my $self=shift;
	my $normalizedTheta=shift;
	return $self->{theta1} + $self->{delta_theta}*$normalizedTheta;
	}
sub isWithinThetaRange {
	my $self=shift;
	my $theta=shift;
	#print "iswin stuff: $self->{theta1} , $theta , $self->{theta2}\n";
	if ($self->{large_arc_flag}==0) {
		if ($self->{sweep_flag} == 0) {
			return (($theta < $self->{theta1} || $theta eq $self->{theta1}) && ($theta > $self->{theta2} || $theta eq $self->{theta2})) ? 1:0;
			}
		else {
			return (($theta > $self->{theta1} || $theta eq $self->{theta1}) && ($theta < $self->{theta2} || $theta eq $self->{theta2})) ? 1:0;
			}
		}
	else {
		if ($self->{sweep_flag} == 0) {
			return (($theta < $self->{theta1} || $theta eq $self->{theta1}) && ($theta > $self->{theta2} || $theta eq $self->{theta2})) ? 1:0;
			}
		else {
			return (($theta > $self->{theta1} || $theta eq $self->{theta1}) && ($theta < $self->{theta2} || $theta eq $self->{theta2})) ? 1:0;
			}
		}
	}
sub f {
	my $self = shift;
	my $x = shift;
	my @intersections;
	$x-=$self->{cx};
	if ($x < -$self->{rx} || $x > $self->{rx}) {return;}

	for (my $i=0;$i<scalar(@{$self->{fxdangerranges}});$i++) {
		if (($x > $self->{'fxdangerranges'}->[$i]->[0] || $x eq $self->{'fxdangerranges'}->[$i]->[0]) && ($x < $self->{'fxdangerranges'}->[$i]->[1] || $x eq $self->{'fxdangerranges'}->[$i]->[1]) && !ref($x)) {
			print "DANGER ZONE FOR f(x) (arc)! x:$x\n";
			$x=Math::BigFloat->new($x) if !ref($x);
			}
		}

	my $rot_line_slope=sin($pi/2 - $self->{phi_radians})/cos($pi/2 - $self->{phi_radians});
	#print "rot_line_slope:$rot_line_slope\n";
	if (abs($rot_line_slope) > 1.0 * 10**6 || $rot_line_slope eq 'inf' || $rot_line_slope eq '-inf') {
		my $y=sqrt($self->{ry}**2 * (1 - ($x**2)/($self->{rx}**2)));#vertical line. use ellipse formula to get the +/- y vals
		push(@intersections,[$x,$y],[$x,-$y]);
		}
	else {

		my $rot_line = _rotate2d([0,0],[$x,0],-$self->{phi_radians}); # point on a vertical x=C line getting tilted into ellipse frame, where the line will have a slope equal to -tan(phi)

		#print "phi : $self->{phi_radians}\nsin(phi)/cos(phi):",sin($self->{phi_radians}),"/",cos($self->{phi_radians}),"\n";
		#print "rot line: [$rot_line->[0],$rot_line->[1]]\n";

		my $a = (($rot_line_slope)**2/$self->{ry}**2) + 1/$self->{rx}**2;
		my $b = ( 2 * ($rot_line_slope) * ($rot_line->[1] - ($rot_line_slope)*$rot_line->[0]))/$self->{ry}**2;
		my $c =(($rot_line->[1] - ($rot_line_slope)*$rot_line->[0])**2 / $self->{ry}**2 ) - 1;
		#print "quad coeffs: $a, $b, $c\n";
		my @xs = &quadraticformula($a,$b,$c,1);
		#print "solution(s) from quad form:",join(",",@xs),"\n";
		for (my $i=0;$i<@xs;$i++) {
			my $y=$rot_line_slope * $xs[$i] + ($rot_line->[1] - $rot_line_slope * $rot_line->[0]); #line formula
			push( @intersections,
				[$xs[$i],$y],
				);
			}
		}


	#for (my $i=0;$i<@intersections;$i++) {
	#	print "centered, unrotated intersection $i: [$intersections[$i]->[0],$intersections[$i]->[1]]\n";
	#	}
	for (my $i=0;$i<@intersections;$i++) {
		my $h=sqrt($intersections[$i]->[0]**2 + $intersections[$i]->[1]**2);
		$intersections[$i] = _rotate2d([0,0],$intersections[$i],$self->{phi_radians});
		$intersections[$i]->[0]+=$self->{cx};
		$intersections[$i]->[1]+=$self->{cy};
		}
	#Now check to see of those intersections are within bounds - within sweep

	my $leg1;
	my $leg2;
	if ($self->{large_arc_flag}==0) {
		if ($self->{sweep_flag} == 0) {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			}
		else {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			}
		}
	else {
		if ($self->{sweep_flag} == 0) {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			}
		else {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			}
		}
	@intersections = grep { ($self->{large_arc_flag} && !$self->isWithinSweep($_,$leg1,$leg2)) || (!$self->{large_arc_flag} && $self->isWithinSweep($_,$leg1,$leg2)) } @intersections;
	return wantarray ? (map {$_->[1]} @intersections) : $intersections[0]->[1];
	}

sub F {
	my $self = shift;
	my $y = shift;
	my @intersections;
	$y-=$self->{cy};
	if ($y < -$self->{ry} || $y > $self->{ry}) {return;}

	for (my $i=0;$i<scalar(@{$self->{Fydangerranges}});$i++) {
		if (($y > $self->{Fydangerranges}->[$i]->[0] || $y eq $self->{Fydangerranges}->[$i]->[0]) && ($y < $self->{Fydangerranges}->[$i]->[1] || $y eq $self->{Fydangerranges}->[$i]->[1]) && !ref($y)) {
			print "DANGER ZONE FOR F(y) (arc) ! y:$y ";
			if ($Math::MikePath::enableCarefulFofy) {$y=Math::BigFloat->new(''.$y) if !ref($y);}
			else {
				print " BUT ignoring because \$Math::MikePath::enableCarefulFofy = $Math::MikePath::enableCarefulFofy";
				}
			print "\n";
			}
		}

	my $rot_line_slope=sin(-$self->{phi_radians})/cos(-$self->{phi_radians});
	#print "rot_line_slope:$rot_line_slope\n";
	if (abs($rot_line_slope) > 1.0 * 10**6 || $rot_line_slope eq 'inf' || $rot_line_slope eq '-inf' || abs($rot_line_slope) < 1.0 * 10**-10) {
		#print "in - fuuhh - nit\n";
		my $x=sqrt($self->{rx}**2 * (1 - ($y**2)/($self->{ry}**2)));#vertical line. use ellipse formula to get the +/- y vals
		push(@intersections,[$x,$y],[-$x,$y]);
		}
	elsif (abs($rot_line_slope) < 1.0 * 10**-10) {
		my $x=sqrt($self->{rx}**2 * (1 - ($y**2)/($self->{ry}**2)));#vertical line. use ellipse formula to get the +/- y vals
		push(@intersections,[$x,$y],[-$x,$y]);
		}
	else {

		my $rot_line = _rotate2d([0,0],[0,$y],-$self->{phi_radians}); # point on a vertical x=C line getting tilted into ellipse frame, where the line will have a slope equal to -tan(phi)

		#print "phi : $self->{phi_radians}\nsin(phi)/cos(phi):",sin($self->{phi_radians}),"/",cos($self->{phi_radians}),"\n";
		#print "rot line: [$rot_line->[0],$rot_line->[1]]\n";

		my $a = (1/$self->{ry}**2) + 1/($self->{rx}**2 * $rot_line_slope**2);
		my $b = (2*($rot_line->[0] - ($rot_line->[1]/$rot_line_slope)))/($self->{rx}**2 * $rot_line_slope);
		my $c = (($rot_line->[0] - ($rot_line->[1]/$rot_line_slope))**2 / $self->{rx}**2) - 1;
		#print "quad coeffs: $a, $b, $c\n";
		my @ys = &quadraticformula($a,$b,$c,1);
		#print "solution(s) from quad form:",join(",",@xs),"\n";
		for (my $i=0;$i<@ys;$i++) {
			#my $x=$rot_line_slope * $ys[$i] + ($rot_line->[0] - $rot_line_slope * $rot_line->[1]); #line formula
			my $x=(($ys[$i] - $rot_line->[1])/$rot_line_slope) + $rot_line->[0]; #line formula
			push( @intersections,
				[$x,$ys[$i]],
				);
			}
		}


	for (my $i=0;$i<@intersections;$i++) {
		my $h=sqrt($intersections[$i]->[0]**2 + $intersections[$i]->[1]**2);
		$intersections[$i] = _rotate2d([0,0],$intersections[$i],$self->{phi_radians});
		$intersections[$i]->[0]+=$self->{cx};
		$intersections[$i]->[1]+=$self->{cy};
		}
	#print "candidates:    ",join("\n               ",map {"[$_->[0],$_->[1]]"} @intersections),"\n";
	#Now check to see of those intersections are within bounds - within sweep

	my $leg1;
	my $leg2;
	if ($self->{large_arc_flag}==0) {
		if ($self->{sweep_flag} == 0) {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			}
		else {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			}
		}
	else {
		if ($self->{sweep_flag} == 0) {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			}
		else {
			$leg1=[[$self->{cx},$self->{cy}],[$self->{p1}->[0],$self->{p1}->[1]]];
			$leg2=[[$self->{cx},$self->{cy}],[$self->{p2}->[0],$self->{p2}->[1]]];
			}
		}
	@intersections = grep { ($self->{large_arc_flag} && !$self->isWithinSweep($_,$leg1,$leg2)) || (!$self->{large_arc_flag} && $self->isWithinSweep($_,$leg1,$leg2)) } @intersections;
	return wantarray ? (map {$_->[0]} @intersections) : $intersections[0]->[0];
	}
sub point {
	my $self = shift;
	my $theta = shift;

    my $arc_theta = $self->normalizedThetaToArcTheta($theta);
	#print "test:::: ",($self->isWithinThetaRange($arc_theta)?1:0),"\n";
	return if !$self->isWithinThetaRange($arc_theta);
	for (my $i=0;$i<scalar(@{$self->{mxidangerranges}});$i++) {
		if (($theta > $self->{mxidangerranges}->[$i]->[0] || $theta eq $self->{mxidangerranges}->[$i]->[0]) && ($theta < $self->{mxidangerranges}->[$i]->[1] || $theta eq $self->{mxidangerranges}->[$i]->[1]) && !ref($theta)) {
			#print "DANGER ZONE FOR point(theta) for x. $theta:ref?:",ref($theta),"\n";
			#print "DZ  theta for x: $theta\n";
			$theta=Math::BigFloat->new($theta) if !ref($theta);
			#print "DZB theta: $theta\n";
			}
		}

	for (my $i=0;$i<scalar(@{$self->{myidangerranges}});$i++) {
		if (($theta > $self->{myidangerranges}->[$i]->[0] || $theta eq $self->{myidangerranges}->[$i]->[0]) && ($theta < $self->{myidangerranges}->[$i]->[1] || $theta eq $self->{myidangerranges}->[$i]->[1]) && !ref($theta)) {
			#print "DANGER ZONE FOR point(theta) for y. $theta:$theta\n";
			#print "DZ  theta for y: $theta\n";
			$theta=Math::BigFloat->new($theta) if !ref($theta);
			}
		}

	#ellipse formulas derived from SVG spec
	# x = rx * cos(theta) * cos(phi) + ry * sin(theta) * -sin(phi) + Cx
	# y = rx * cos(theta) * sin(phi) + ry * sin(theta) *  cos(phi) + Cy

    # We had a BigFloat version of the math here, but it was commented out
    # and looked messy and unfinished, so deleted it.
    # You may need to reintroduce that at some point.
    # BigFloat math was done if $theta was a BigFloat.
    # Until that's needed again, downgrade any $theta that's a BigFloat.

	if (ref($theta)) {$theta=0 + sprintf("%.18f",$theta->bstr);}
	my $ct=cos($arc_theta);
	my $st=sin($arc_theta);
	return [$self->{rx} * $ct * cos($self->{phi_radians}) + $self->{ry} * $st * -sin($self->{phi_radians}) + $self->{cx},
	        $self->{rx} * $ct * sin($self->{phi_radians}) + $self->{ry} * $st *  cos($self->{phi_radians}) + $self->{cy}];

	}

# SPEED ATTEMPTS - IN THE SEVERAL SUBS BELOW, WHEREVER YOU SEE (0 + SOMETHING)
#                  IT USED TO BE eval(SOMETHING), BUT WE'RE GETTING RID OF eval()s
#                  IS THE 0 + USEFUL? NEEDED? PROBABLY NOT, BUT WE'RE NOT GOING TO
#                  CHECK RIGHT NOW, SO GO WITH GENERIC NUMIFY APPROACH OF + 0.

sub evalYofTheta {
	my $self = shift;
	#print "evalYofTheta     theta: $_[0]\n";
	my $theta = $self->normalizedThetaToArcTheta(shift);
	#print "evalYofTheta arc theta: $theta\n";
	#if (!$self->isWithinThetaRange($theta)) {print "evalYofTheta (OUT OF RANGE)\n";}
	return if !$self->isWithinThetaRange($theta);
	#my $ret=$self->{rx} * eval(sprintf("%.14f",cos($theta))) * sin($self->{phi_radians}) + $self->{ry} * eval(sprintf("%.14f",sin($theta))) *  cos($self->{phi_radians}) + $self->{cy};
	#print "evalYofTheta $ret=$self->{rx} * cos($theta) * sin($self->{phi_radians}) + $self->{ry} * sin($theta) *  cos($self->{phi_radians}) + $self->{cy};\n";
	return $self->{rx} * (0 + sprintf("%.14f",cos($theta))) * sin($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",sin($theta))) *  cos($self->{phi_radians}) + $self->{cy};
	}
# same as above, but provided theta is alread the arc theta and not the normalized theta
# so should probably have above jjust do the theat conversion then call this
sub evalYofArcTheta {
	my $self = shift;
	my $theta = shift;
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",cos($theta))) * sin($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",sin($theta))) *  cos($self->{phi_radians}) + $self->{cy};
	}
sub evalXofTheta {
	my $self = shift;
	my $theta = $self->normalizedThetaToArcTheta(shift);
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",cos($theta))) * cos($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",sin($theta))) * -sin($self->{phi_radians}) + $self->{cx};
	}
# same as above, but provided theta is alread the arc theta and not the normalized theta
# so should probably have above jjust do the theat conversion then call this
sub evalXofArcTheta {
	my $self = shift;
	my $theta = shift;
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",cos($theta))) * cos($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",sin($theta))) * -sin($self->{phi_radians}) + $self->{cx};
	}
sub evalYPrimeofTheta {
	my $self = shift;
	my $theta = $self->normalizedThetaToArcTheta(shift);
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",-sin($theta))) * sin($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",cos($theta))) *  cos($self->{phi_radians});
	}
sub evalXPrimeofTheta {
	my $self = shift;
	my $theta = $self->normalizedThetaToArcTheta(shift);
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",-sin($theta))) * cos($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",cos($theta))) * -sin($self->{phi_radians});
	}
sub evalYDoublePrimeofTheta {
	my $self = shift;
	my $theta = $self->normalizedThetaToArcTheta(shift);
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",-cos($theta))) * sin($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",-sin($theta))) *  cos($self->{phi_radians});
	}
sub evalXDoublePrimeofTheta {
	my $self = shift;
	my $theta = $self->normalizedThetaToArcTheta(shift);
	return if !$self->isWithinThetaRange($theta);
	return $self->{rx} * (0 + sprintf("%.14f",-cos($theta))) * cos($self->{phi_radians}) + $self->{ry} * (0 + sprintf("%.14f",-sin($theta))) * -sin($self->{phi_radians});
	}
sub solveYforTheta {
	my $self = shift;
	my $y = shift;
	my @xs = $self->F($y);
	my @ts;
	for (my $i=0;$i<@xs;$i++) {
		my $unrot=_rotate2d([$self->{cx},$self->{cy}],[$xs[$i],$y],-$self->{phi_radians});
		$unrot->[0] -= $self->{cx};
		$unrot->[1] -= $self->{cy};
		#now corresponding ellipse formulas are
		#x=$self->{rx} * cos(theta);
		#y=$self->{ry} * sin(theta);
		#so that's solvable for theta, right?
		#y=$self->{ry} * sin(theta);
		#theta=asin(y/ry);


		my $t=asin($unrot->[1]/$self->{ry});

# SIMILAR TO WHAT WE DEBUGGED FIXED FOR solveXforTheta BELOW, BUT UNTESTED/VERIFIED HERE SO FAR
# 5/10/2017
$t*=-1 if $self->{sweep_flag};


		my $other_t=$t + (($t>0)?-1:1) * $pi;
		my $other_t2=$t + (($t>0)?-1:1) * 2*$pi;
		my $other_t3=$other_t + (($other_t>0)?-1:1) * 2*$pi;
		push(@ts,$t,$other_t,$other_t2,$other_t3);
		}
	return map {$self->arcThetaToNormalizedTheta($_)} grep {$self->isWithinThetaRange($_) && abs($self->evalYofTheta($_) - $y)<0.0000001} @ts;
	}
sub solveXforTheta {
	my $self = shift;
	my $x = shift;
	my @ys = $self->f($x);
    #warn "solveXforTheta for \np1: [$self->{p1}->[0],$self->{p1}->[1]]\np2: [$self->{p2}->[0],$self->{p2}->[1]]\n x: $x\nys: ",join(',',@ys),"\n";
    #warn "arc ends calced from self figured arc theta bounds: \n";
    #warn "theta1: $self->{theta1} end: ",$self->evalXofArcTheta($self->{theta1}),", ",$self->evalYofArcTheta($self->{theta1}),"\n";
    #warn "theta2: $self->{theta2} end: ",$self->evalXofArcTheta($self->{theta2}),", ",$self->evalYofArcTheta($self->{theta2}),"\n";
	my @ts;
	for (my $i=0;$i<@ys;$i++) {
		my $unrot=_rotate2d([$self->{cx},$self->{cy}],[$x,$ys[$i]],-$self->{phi_radians});
        #warn "unrot same?:\n$unrot->[0],$unrot->[1] sameas\n$x,$ys[$i]\n";
		$unrot->[0] -= $self->{cx};
		$unrot->[1] -= $self->{cy};
        #warn "aftershift?:\n$unrot->[0],$unrot->[1]\n [center was $self->{cx},$self->{cy}]\n";

		#now corresponding ellipse formulas are
		#x=$self->{rx} * cos(theta);
		#y=$self->{ry} * sin(theta);
		#so that's solvable for theta, right?
		#x=$self->{rx} * cos(theta);
		#theta=acos(x/rx);
		#theta=pi/2 - asin(x/rx)

# DO WE NEED TO DO A DIFF CALC IF SWEEP FLAG IS ONE WAY OR THE OTHER?
# 5/10/2017

		my $t=($pi/2) - asin($unrot->[0]/$self->{rx});

# NOT SURE ABOUT THIS, BUT MAYBE - looking good! for test problem case
# could be I only ever used this before with sweep_flag == 0 type arcs?
# 5/10/2017
$t*=-1 if $self->{sweep_flag};

# TODO OOOOOOOOO!
# MAKE SURE ALL THIS IS SIMILAR IN solveYforTheta ABOVE AND IN JAVASCRIPT VERSION

		my $other_t  =       $t + (($t      >0) ?-1:1) *   $pi;
		my $other_t2 =       $t + (($t      >0) ?-1:1) * 2*$pi;
		my $other_t3 = $other_t + (($other_t>0) ?-1:1) * 2*$pi;

		push(@ts,$t,$other_t,$other_t2,$other_t3);
		}

    #warn "theta candidates: \n",
    #    join("\n",map {
    #        $_.' nrm:'.$self->arcThetaToNormalizedTheta($_).' '
    #        .($self->isWithinThetaRange($_)?'win':'NOTwin').' ['.$self->{theta1}.', '.$self->{theta2}.'] '
    #        .abs($self->evalXofArcTheta($_) - $x).'<?0.0000001 '
    #        ."\n".$self->evalXofArcTheta($_).','.$self->evalYofArcTheta($_)."\n"
    #        } @ts),"\n";

	return map  {$self->arcThetaToNormalizedTheta($_)}
	       grep {   $self->isWithinThetaRange($_)
	             && abs($self->evalXofArcTheta($_) - $x)<0.0000001
	            } @ts;
	}
sub solveYPrimeforTheta {
	my $self = shift;
	my $yp = shift;
	#first of all, that slope would be this slope, if ellipse were "unrotated":
	#yp_unrot = tan(atan(yp) - self->phi)
	my $yp_unrot = sin(atan2($yp,1) - $self->{phi_radians})/cos(atan2($yp,1) - $self->{phi_radians});
	#the unrotated, centered ellipse formulas are
	#x=rx * cos(theta)
	#y=ry * sin(theta)
	#derivatives of those, with respect to theta:
	#x'=rx * -sin(theta)
	#y'=ry * cos(theta)
	#and dy/dx is
	#(dy/dt) / (dx/dt)
	#so you want to solve this for theta:
	#yp_unrot = (ry * cos(theta)) / (rx * -sin(theta))
	#which I hope comes out this simply:
	#theta = atan((ry/-rx) * 1/yp_unrot)
	my $t = atan2($self->{ry},-$self->{rx} * $yp_unrot);
	my $other_t=$t + (($t>0 || $t eq 0)?-1:1) * $pi;
	my $other_t2=$t + (($t>0 || $t eq 0)?-1:1) * 2*$pi;
	my $other_t3=$other_t + (($other_t>0 || $other_t eq 0)?-1:1) * 2*$pi;
	return map {$self->arcThetaToNormalizedTheta($_)} grep {$self->isWithinThetaRange($_)} ($t,$other_t,$other_t2,$other_t3);
	}
sub solveXPrimeforTheta {
	my $self = shift;
	my $xp = shift;
	#first of all, that slope would be this slope, if ellipse were "unrotated":
	#xp_unrot = tan(atan(xp) - self->phi)
	my $xp_unrot = sin(atan2($xp,1) - $self->{phi_radians})/cos(atan2($xp,1) - $self->{phi_radians});
	#the unrotated, centered ellipse formulas are
	#x=rx * cos(theta)
	#y=ry * sin(theta)
	#derivatives of those, with respect to theta:
	#x'=rx * -sin(theta)
	#y'=ry * cos(theta)
	#and dx/dy is
	#(dx/dt) / (dy/dt)
	#so you want to solve this for theta:
	#xp_unrot = (rx * -sin(theta)) / (ry * cos(theta))
	#which I hope comes out this simply:
	#theta = atan((ry*xp_unrot)/-rx/)
	my $t = atan2($self->{ry}*$xp_unrot,-$self->{rx});
	my $other_t=$t + (($t>0 || $t eq 0)?-1:1) * $pi;
	my $other_t2=$t + (($t>0 || $t eq 0)?-1:1) * 2*$pi;
	my $other_t3=$other_t + (($other_t>0 || $other_t eq 0)?-1:1) * 2*$pi;
	return map {$self->arcThetaToNormalizedTheta($_)} grep {$self->isWithinThetaRange($_)} ($t,$other_t,$other_t2,$other_t3);
	}

sub getLength {
	my $self = shift;
	my $res=shift;
	my $start_theta = shift;
	my $end_theta = shift;
	#also need to take theta range options for arc lengths
	if (!defined($res)) {$res=1000;}
#	if (!defined($start_theta)) {$start_theta=$self->{theta1};}
#	if (!defined($end_theta)) {$end_theta=$self->{theta2};}
	if (!defined($start_theta)) {$start_theta=0;}
	if (!defined($end_theta)) {$end_theta=1;}



	# if the two radii are equal, it's a circular arc
	# (which is usually how I use this ellipse stuff)
	# and we have 10th grade math for that
	if ($self->{rx} eq $self->{ry}) {
		#warn "arc length: ",($self->{rx} * $self->{delta_theta} * ($end_theta - $start_theta))," = $self->{rx} * $self->{delta_theta} * ($end_theta - $start_theta)";
		return abs($self->{rx} * $self->{delta_theta} * ($end_theta - $start_theta));
		}

# this isn't set up yet to take arbitrary thetas for sub-lengths of ellipse arc

	my $sum=0;
	my $point1=$self->point($start_theta);
	my $point2;
# here's the problem: for anything but the full arc from theta1 to theta2, using $self->{delta_theta} is wrong
# - you need to calc a different delta - and if you use full ellipse angles, you don't know right off if
# you should use the big delta or the small one - and you would need to screen the angles passed in,
# and what if one of them was out of range? etc.
#	my $thetainc=$self->{delta_theta}/$res;
	my $thetainc=1/$res;
	for (my $i=0;$i<$res;$i++) {
		$point2=$point1;
		#$point1=$self->point($self->{theta1}+$i*$thetainc);
		$point1=$self->point($start_theta+$i*$thetainc);
		$sum+=sqrt(($point2->[0]-$point1->[0])**2 + ($point2->[1]-$point1->[1])**2);
		}
	return $sum;
#Those comments might not be right. I think this might work now with the alt parameterization I did for arcs. Still suspect until verified working, but glancing at it now, seems like it might be right and working.
	}

sub getFeet {
	my $self=shift;
	my $x=shift;
	my $y=shift;
	#for each interval between critical points - critical due to features of x(i) and y(i). - hueristic and then root find to get any 90 degree intersections
	my @feet=();
	my %dupsieve_i_set;
	my @extreme_i_set= grep {!$dupsieve_i_set{$_}++} sort {$a<=>$b} (@{$self->{extremeys_is}},@{$self->{extremexs_is}});
	for (my $i=@extreme_i_set - 1;$i>0;$i--) {
		splice(@extreme_i_set,$i,0,(($extreme_i_set[$i]+$extreme_i_set[$i - 1])/2))
		}

	for (my $i=1;$i<@extreme_i_set;$i++) {
		#print "ref in extreme is? " , (ref($extreme_i_set[$i - 1]) || ref($extreme_i_set[$i])?'yes':'no') , "\n";
		next if ($extreme_i_set[$i - 1] eq $extreme_i_set[$i]);
		my $boundl=$extreme_i_set[$i - 1];
		my $boundr=$extreme_i_set[$i];
		#print "\nx:$x y:$y BOUNDS: $boundl , $boundr\n";
		my $find90 = sub {
			#dotproduct equals zero for two perpendicular vectors
			my $rayvec=[$self->evalXofTheta($_[0]) - $x,$self->evalYofTheta($_[0]) - $y];
			my $tanvec = [1,0 + $self->slopeTangent_byTheta($_[0])];
			my $ret = $rayvec->[0] * $tanvec->[0] + $rayvec->[1] * $tanvec->[1];
			return $ret;
			};
		my $find90_2 = sub {
			#dotproduct equals zero for two perpendicular vectors
			my $rayvec=[$self->evalYofTheta($_[0]) - $y,-1 * ($self->evalXofTheta($_[0]) - $x)];
			my $tanvec = [1,0 + $self->slopeNormal_byTheta($_[0])];
			my $ret = $rayvec->[0] * $tanvec->[0] + $rayvec->[1] * $tanvec->[1];
			return $ret;
			};
		my $subtouse=$find90;
		if (abs(cos($self->angleTangent_byTheta($boundl))) < 0.001 || abs(cos($self->angleTangent_byTheta($boundr))) < 0.001) {
			#print "using find90_2\n";
			$subtouse=$find90_2;
			}
		else {
			#print "using find90\n";
			}
		my ($foot_i,$msg);
		#print "before brnets\n";
		($foot_i,$msg)=BrentsMethod($subtouse,[$boundl,$boundr],$self->{precision},undef,'trying to find feet on Elliptical Arc in MikePath with precision:'.($self->{precision}).'');
		#print "after brnets\n";
		if (!defined($msg)) {
			#print "ref foot_i?: ",(ref($foot_i)?'YES':'no'),"\n";
			#print "i for foot:$foot_i [",$self->evalXofTheta($foot_i),",",$self->evalYofTheta($foot_i),"]\n";
			#push(@feet,[$self->evalXofTheta($foot_i),$self->evalYofTheta($foot_i)]);
			#print "pre pUSH\n";
			my $retpoint = $self->point($foot_i);
			push(@{$retpoint},$foot_i);
			push(@feet,$retpoint);
			}
		#else {print "getfeetforarcfailure: $msg\n";}
		}
	return @feet;
	}
sub angleTangent_byTheta {
	my $self = shift;
	my $theta = shift;
	return if !$self->isWithinThetaRange($self->normalizedThetaToArcTheta($theta));
	my @ret=$self->angleTangent(undef,undef,$theta);
	return wantarray ? @ret : $ret[0];
	}
sub angleNormal_byTheta {
	my $self = shift;
	my $theta = shift;
	return if !$self->isWithinThetaRange($self->normalizedThetaToArcTheta($theta));
	my @ret=$self->angleNormal(undef,undef,$theta);
	return wantarray ? @ret : $ret[0];
	}
sub slopeTangent_byTheta {
	my $self = shift;
	my $theta = shift;
	return if !$self->isWithinThetaRange($self->normalizedThetaToArcTheta($theta));
	my $yp=$self->evalYPrimeofTheta($theta);
	my $xp=$self->evalXPrimeofTheta($theta);
	if ($xp eq '0') {return (($yp < 0)?'-':'+').'inf';}
	if ($yp eq '0') {return (($xp < 0)?'-':'+').'0';}
	else {
		#print "simpdiv: ",($yp/$xp),"\n";
		#print "atan2(): ",atan2($yp,$xp),"\n";
		#print "comps  : ",$yp,",",$xp,"\n";
		return $yp/$xp;
		}
	}

sub slopeNormal_byTheta {
	my $self = shift;
	my $theta = shift;
	return if !$self->isWithinThetaRange($self->normalizedThetaToArcTheta($theta));
	my $slopeTangent = $self->slopeTangent_byTheta($theta);
	if ($slopeTangent =~ /([\-\+]?)inf$/i) {
		my $sign = '';
		if (length($1)) {if ($1 eq '-') {$sign='+';} else {$sign='-';}}
		return $sign.'0';
		}
	elsif ($slopeTangent =~ /^([\-\+]?)0$/) {
		my $sign = '';
		if (length($1)) {if ($1 eq '-') {$sign='+';} else {$sign='-';}}
		return $sign.'inf';
		}
	else {
		return -1/$slopeTangent;
		}
	}
sub angleTangent {
	my $self = shift;
	my $x    = shift;
	my $y    = @_?shift:undef;
	my $theta= @_?shift:undef;
	#get intersect points
	my @intersects;
	if (defined($x)) {my @ys;@ys=$self->f($x);foreach (sort {$a<=>$b} @ys) {push(@intersects,[$x,$_])}}
	elsif (defined($y)) {my @xs;@xs=$self->F($y);foreach (sort {$a<=>$b} @xs) {push(@intersects,[$_,$y])}}
	elsif (defined($theta)) {push(@intersects,$self->point($theta));}
	#then use the foci calculated in the ellipse setup
	# and the info here: http://mysite.du.edu/~jcalvert/math/ellipse.htm
	# to make lines and figure angles to get tangent angle...

	#The tangent line at point P on the ellipse is perpendicular to the line bisecting
	#the angle between the two lines extending from point P to the two foci of the ellipse. (So the bisector is the normal.)
	#That angle is given by the arccosine of the dot product over the product of the magnitude of the vectors (lines) between the two lines:
	# arccos( (line1 dot line2) / (|line1|*|line2|) )
	#arccos(x) is eqivalent to pi/2 - arcsin(x)

	#really you're calculating an inward pointing normal angle and adding 90 deg to get the tangent

	# ... much later : but you should take sweep_flag into account
	#    The elliptical arcs here have direction - start point and end point - and a tangent should go in start-to-end direction of elliptical path
	#    and a normal should point off to the right
	#    so added one sweep flag controlled * 1 or -1 in this stuff, and took a negative sign off the cos/sin in slopeNormal function
	#    and that looks right on my test page

	my @ret;
	for (my $i=0;$i<@intersects;$i++) {
		my $line1=[$intersects[$i],$self->{f1}];
		my $line2=[$intersects[$i],$self->{f2}];
		my $a1 = atan2($line1->[1]->[1] - $line1->[0]->[1],$line1->[1]->[0] - $line1->[0]->[0]);
		my $a2 = atan2($line2->[1]->[1] - $line2->[0]->[1],$line2->[1]->[0] - $line2->[0]->[0]);
		#push(@ret,($a2 + $a1)/2);
		#print "$a1 and $a2 and minus:",($a2 - $a1),"\n";
		#print " dot ",($pi/2 - asin((($line1->[1]->[0] - $line1->[0]->[0]) * ($line2->[1]->[0] - $line2->[0]->[0]) + ($line1->[1]->[1] - $line1->[0]->[1]) * ($line2->[1]->[1] - $line2->[0]->[1]))/(sqrt(($line1->[1]->[1] - $line1->[0]->[1])**2 + ($line1->[1]->[0] - $line1->[0]->[0])**2)*sqrt(($line2->[1]->[1] - $line2->[0]->[1])**2 + ($line2->[1]->[0] - $line2->[0]->[0])**2)))),"\n";
		push(@ret,
			$pi/2 * ($self->{sweep_flag}?-1:1) + #add +/- 90 deg from the normal angle you calculate below to get the tangent
			$a1 + #angle of the line/vector from point P on ellipse to focus 1
			(($a2 - $a1)>0 || ($a2 - $a1) eq 0?1:-1) * # hmm..., whether we add or subtract the half angle below from the line1 angle. Is this okay?
			0.5 * # this and below calculated half the angle between the two lines
			($pi/2 -
				asin(
						(
							($line1->[1]->[0] - $line1->[0]->[0]) * ($line2->[1]->[0] - $line2->[0]->[0])  +
							($line1->[1]->[1] - $line1->[0]->[1]) * ($line2->[1]->[1] - $line2->[0]->[1])
						) /
						(
							sqrt(($line1->[1]->[1] - $line1->[0]->[1])**2 + ($line1->[1]->[0] - $line1->[0]->[0])**2) *
							sqrt(($line2->[1]->[1] - $line2->[0]->[1])**2 + ($line2->[1]->[0] - $line2->[0]->[0])**2)
						)
					)
				)
			);
		}
	@ret = map {angle_reduce($_)} @ret;
	return wantarray ? @ret : $ret[0];
	}
sub secondDerivative {
    my $self = shift;
    die "Need to work out second derivative with respect to x for elliptical arc when rx != ry";
    }
sub slopeTangent {my @ats;@ats=$_[0]->angleTangent($_[1],$_[2],$_[3]);my @ret;for (my $i=0;$i<@ats;$i++) {push(@ret, sin($ats[$i])/cos($ats[$i]))} return wantarray ? @ret : $ret[0];}
sub slopeNormal  {my @ats;@ats=$_[0]->angleTangent($_[1],$_[2],$_[3]);my @ret;for (my $i=0;$i<@ats;$i++) {push(@ret, cos($ats[$i])/sin($ats[$i]))} return wantarray ? @ret : $ret[0];}
sub angleNormal  {my @ret = (map {$_ - $pi/2} $_[0]->angleTangent($_[1],$_[2],$_[3]));@ret = map {angle_reduce($_)} @ret;return wantarray ? @ret : $ret[0];}


sub isWithinSweep {
	my $self=shift;
	my $p=shift;
	my $leg1=shift;
	my $leg2=shift;
	my $leftness_1 = _howleft([[$leg1->[0]->[0],$leg1->[0]->[1]],[$leg1->[1]->[0],$leg1->[1]->[1]]],[$p->[0],$p->[1]]);
	my $leftness_2 = _howleft([[$leg2->[0]->[0],$leg2->[0]->[1]],[$leg2->[1]->[0],$leg2->[1]->[1]]],[$p->[0],$p->[1]]);
	#print "\niws: $leftness_1 * $leftness_2 <0 ?\n";
	#if ($leftness_1*$leftness_2 < 0 || $leftness_1==0 || $leftness_2==0) {return 1;}
	#print "iswithinsweep tests:(($leftness_1 < 0 && $leftness_2 > 0) || $leftness_1==0 || $leftness_2==0)\n";
	if (($leftness_1 < 0 && $leftness_2 > 0) || $leftness_1==0 || $leftness_2==0) {return 1;}
	else {return 0;}

	}
sub _howleft { #just copied from CAD::Calc
	my ($line, $pt) = @_;
	my $isleft = ($line->[1]->[0] - $line->[0]->[0]) *
					    ($pt->[1] - $line->[0]->[1]) -
				 ($line->[1]->[1] - $line->[0]->[1]) *
				 	    ($pt->[0] - $line->[0]->[0]);
	return($isleft);
	}
sub _rotate2d {
	my $origin=shift;
	my $point=shift;
	my $angle=shift;
	my $dx=($point->[0]-$origin->[0]);
	my $dy=($point->[1]-$origin->[1]);
	#$angle = ($angle - (2*$pi)*int($angle/(2*$pi)));#usually built into the trig functions
	#{a c-b d, a d+b c}
	return [$origin->[0] + ($dx*cos($angle) - $dy*sin($angle)),$origin->[1] + ($dx*sin($angle) + $dy*cos($angle))];
	}
sub asin {
	#Based on Wolfram MathWorld
	#http://mathworld.wolfram.com/InverseSine.html
	#but it's supposed to use complex numbers, hmmm
	if ($_[0] eq -1)    {return $pi/2 * -1} #eq 2
	elsif ($_[0] eq 1)  {return $pi/2     } #eq 4
	elsif ($_[0] eq 0)  {return 0;             } #eq 3
	else {                                       #eq 15
# SPEED ATTEMPTS - why was this eval() here?
#		if (eval(abs($_[0]))>1) {warn("giving something bigger than +/-1 to your asin:",$_[0],"\n");}
		if (abs($_[0])>1) {warn("giving something bigger than +/-1 to your asin:",$_[0],"\n");}
		return atan2($_[0],sqrt(1-$_[0]**2));
		}
	}
sub angle_reduce {# Copied from CAD::Calc
	my $ang = shift;
	my $angbef = $ang;
	while($ang > $pi) {
		$ang -= 2*$pi;
	}
	while($ang <= -$pi) {
		$ang += 2*$pi;
	}
	#if ($angbef != $ang) {print "reduced angle: from $angbef to $ang\n";}
	return($ang);
	}
sub dimensionalStepFromTheta {

	#same code as for bezier
	#and maybe even line segment if you've been to lazy to do a simpler version for the simple line case
	#it's been fast so far
	#didn't test arc case after pasting this, but line segment case worked with no mods, so this
	#has a good chance of working too.

	my $self=shift;

	my $dim=shift;
	my $theta=shift;
	my $direction=scalar(@_)?shift:1; # 0 or 1

	my $pt_last = $self->point($theta); # shouldn't this be outside the function def?

	my $findnexttheta = sub {
		my $ret;
		if (!ref($_[0])) {
			my $pt_new  = $self->point($_[0]);
			$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2);
			#print "$ret = $dim - CORE::sqrt(($pt_new->[0] - $pt_last->[0])**2 + ($pt_new->[1] - $pt_last->[1])**2) = $ret\n";
			}
		else {
			#warn "I don't think you want to be here - not sure if this mess is debugged.\n";
			my $pt_new  = $self->point($_[0]);
			#print "using BigFloat - \n";
			my $dx=(ref($pt_new->[0]))?$pt_new->[0]->copy()->bsub($pt_last->[0]):$pt_new->[0] - $pt_last->[0];
			my $dxsqrd=(ref($dx))?$dx->bpow(2):$dx**2;
			my $dy=(ref($pt_new->[1]))?$pt_new->[1]->copy()->bsub($pt_last->[1]):$pt_new->[1] - $pt_last->[1];
			my $dysqrd=(ref($dy))?$dy->bpow(2):$dy**2;
			my $distsqrd=(ref($dysqrd))?$dysqrd->copy()->badd($dxsqrd):$dysqrd + $dxsqrd;
			my $dist = (ref($distsqrd))?bigsqrt($distsqrd):sqrt($distsqrd);
			$ret = (ref($dim) ? $dim:Math::BigFloat->new(''.$dim)) - $dist;
			}
		return $ret;
		};

	my $newtheta;
	my $er;
    #warn "$dim , $theta , $direction , $self->{precision}";
	($newtheta,$er) = FalsePosition($findnexttheta,($direction ? [$theta,1]:[0,$theta]),$self->{precision},($direction ? ($theta + (1-$theta)/2):($theta/2)),'dimensionalStepFromTheta for Elliptical Arc segment');
	#print " dim step result ($newtheta,$er)\n";
	if (defined($er)) {
        warn "dimstep er: $er";
		#probably just reached the end
		if (abs(&{$findnexttheta}(($direction ? 1:0))) < $dim) {
			$newtheta=($direction ? 1:0);
			}
		#otherwise the error might be real
		}
	return ($self->point($newtheta),$newtheta);
	}
}
1;