package Math::Function::Root;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(FalsePosition FalsePosition_NoBigFloat Bisection Bisection_NoBigFloat NewtonsMethod BrentsMethod BrentsMethod_NoBigFloatUpgrade max_iter);
use vars qw($Max_Iter $VERSION);
use warnings;
use strict;
use Carp qw(cluck);
#use Math::BigFloat lib => 'GMP';
use Math::BigFloat;
#use Tk;use Tk::LineGraph;use Tk::LineGraphDataset;


#To Do:
#Newton's Method test/debug
#Bisection test/debug
#Brent's Method test/debug

#Do smart switching to BigFloat, possibly limiting decimal places to only what's needed at the time, instead of using default decimal places for every new BigFloat
#Any speed-enhancing optimizations and tricks, without compromising integrity of method
#?conditional method chaining? or is that for another module?
#?add optional method monitoring code that saves all x and f(x) results from iterations
#  and returns or saves them in format suitable for viewing in popular plot viewing apps like gnuplot and Mac Windows apps



$VERSION = '0.1';

$Max_Iter = 50_000;
sub max_iter(;$){
    if( @_ && $_[0] > 0 ){$Max_Iter = shift;}
    return $Max_Iter;
	}

sub FalsePosition {
	my $F = shift;
	my $bounds = shift;
	my $precision = shift;
	my $x = shift; #approximate zero guess
	my $refok = (ref($x) || ref($bounds->[0]) || ref($bounds->[1]) || ref($precision))?1:0;
	my $f = &{$F}($x);
	my $x0=$bounds->[0];
	my $f0=&{$F}($x0);
	if (abs($f) < $precision) {return (ref($x) && !$refok)?$x->bstr:$x;}
	if (abs($f0) < $precision) {return (ref($x0) && !$refok)?$x0->bstr:$x0;}
	if (($f0>0 && $f>0) || ($f0<0 && $f<0)) {
		$x0=$bounds->[1];
		$f0=&{$F}($x0);
		if (abs($f0) < $precision) {return (ref($x0) && !$refok)?$x0->bstr:$x0;}
		}
	if (($f0>0 && $f>0) || ($f0<0 && $f<0)) {
		return wantarray?(undef,'can\'t do False Position method with those bounds ['.$bounds->[0].','.$bounds->[1]."]\n  f(".$bounds->[0].') = '.&{$F}($bounds->[0])."\n  f(".$bounds->[1].') = '.&{$F}($bounds->[1])."\n    and/or that guess\n  f(".$x.') = '.&{$F}($x)."\nwith that precision: $precision\n"):undef;
		}
	my $limit=0;
	for (1..$Max_Iter) {#method of false position (bracketing root finding algorithm)
		my $prex = ref($x)?$x->copy:$x;
		my $denom= $f - $f0;
		if ($denom==0 && (!ref($prex) || !ref($x0))) {$f0=&{$F}(Math::BigFloat->new($x0));$f=&{$F}(Math::BigFloat->new($prex));$denom =  $f - $f0 ;}
		if (!defined($x0)) {die "x0 not defined";}
		if (!defined($f0)) {die "f0 not defined";}
		if (!defined($x)) {die "x not defined";}
		$x= $x0 - ($f0 * (($x - $x0)/$denom));
		$f=&{$F}($x);
		if (abs($f) < $precision) {return (ref($x) && !$refok)?$x->bstr:$x;}
		if (($f0>0 && $f>0) || ($f0<0 && $f<0)) {$x0 = ref($prex)?$prex->copy:$prex;$f0=&{$F}($x0);} #retain last guess that gave function opposite sign
		}
	return wantarray?(undef,'too many iterations in False Position method'):undef;
	}


sub FalsePosition_NoBigFloat {
	my $F = shift;
	my $bounds = shift;
	my $precision = shift;
	my $x = shift; #approximate zero guess
	my $f = &{$F}($x);
	my $x0=$bounds->[0];
	my $f0=&{$F}($x0);
	if (abs($f) < $precision) {return $x;}
	if (abs($f0) < $precision) {return $x0;}
	if (($f0>0 && $f>0) || ($f0<0 && $f<0)) {
		$x0=$bounds->[1];
		$f0=&{$F}($x0);
		if (abs($f0) < $precision) {return $x0;}
		}
	if (($f0>0 && $f>0) || ($f0<0 && $f<0)) {return wantarray?(undef,'can\'t do False Position method with those bounds ['.$bounds->[0].','.$bounds->[1]."]\n  f(".$bounds->[0].') = '.&{$F}($bounds->[0])."\n  f(".$bounds->[1].') = '.&{$F}($bounds->[1])."\n    and/or that guess\n  f(".$x.') = '.&{$F}($x)."\nwith that precision: $precision\n"):undef;}
	my $limit=0;
	for (1..$Max_Iter) {#method of false position (bracketing root finding algorithm)
		my $prex = $x;
		my $denom= $f - $f0;
		if ($denom eq 0) {return wantarray?(undef,"FalsePosition_NoBigFloat got zero denomenator - maybe use the FalsePosition that does bigfloat upgrades\n"):undef;}
		$x= $x0 - ($f0 * (($x - $x0)/$denom));
		$f=&{$F}($x);
		if (abs($f) < $precision) {return $x;}
		if (($f0>0 && $f>0) || ($f0<0 && $f<0)) {$x0 = $prex;$f0=&{$F}($x0);} #retain last guess that gave function opposite sign
		}
	return wantarray?(undef,'too many iterations in False Position method'):undef;
	}


sub Bisection {
    my $f = shift;
	my $bounds = shift;
    my $e = shift;
    my $a = $bounds->[0];
	my $b = $bounds->[1];
	my $p = @_?shift:($a+$b)/2.0; #passed in guess to use as first "midpoint"
	my $refok = (ref($p) || ref($bounds->[0]) || ref($bounds->[1]) || ref($e))?1:0;
	my $py = &{$f}($p);
    my $ay = &{$f}($a);
	my $by = &{$f}($b);
	if (abs($ay) < $e) {return $a;}
	if (abs($by) < $e) {return $b;}
	if (abs($py) < $e) {return $p;}
	my $dobig=0;
    if ( ($ay * $by) > 0 ) {
		return wantarray?(undef,"Bad range: f($a) and f($b) have the same sign: $ay vs $by"):undef;
		}
	my $lastp;
	my $prep;
    for (1..$Max_Iter){
		if( $py * $ay < 0 ){
		    $b = ref($p)?$p->copy:$p;
		    #$by = ref($py)?$py->copy:$py;
			}
		else{
		    $a = ref($p)?$p->copy:$p;
		    $ay = ref($py)?$py->copy:$py;
			}
	#	$lastp=ref($prep)?$prep->copy:$prep;
	#	$prep = ref($p)?$p->copy:$p;
		$p = ($a+$b)/2.0;
		$py = &$f($p);
		if( abs($py) < $e ){return (ref($p) && !$refok)?$p->bstr:$p;}
		if (ref($py) && !ref($p)) {$p=Math::BigFloat->new(''.$p);print "upgraded p to BigFloat because py came out as BigFloat probably meaning you stepped into a danger range\n";}
		
	#	if (!ref($p) && $prep eq $p) {
	#		#return wantarray?((ref($p) && !$refok)?$p->bstr:$p,'bisection ran out of precision : maybe BigFloat would fix this, but usually not'):undef;
	#		print "upgrading to bigFloat in Bisection\n";
	#		$p = Math::BigFloat->new($lastp);
	#		#$p->accuracy(40);
	#		$py = &$f($p);
	#		#$py->accuracy(40);
	#		$dobig = 1;
	#		}
		#print "p:$p , py:$py\n" if $dobig;
		}
	return wantarray?(undef,'Maximum bisection iterations: possible bad solution'):undef;
    }

sub Bisection_NoBigFloat {
    my $f = shift;
	my $bounds = shift;
    my $e = shift;
    my $a = $bounds->[0];
	my $b = $bounds->[1];
	my $p = @_?shift:($a+$b)/2.0; #passed in guess to use as first "midpoint"
	if (ref($p)) {$p=eval($p->bstr);}
	if (ref($bounds->[0])) {$bounds->[0]=eval($bounds->[0]->bstr);}
	if (ref($bounds->[1])) {$bounds->[1]=eval($bounds->[1]->bstr);}
	if (ref($e)) {$e=eval($e->bstr);}
	my $py = &{$f}($p);
    my $ay = &{$f}($a);
	my $by = &{$f}($b);
	if (abs($ay) < $e) {return $a;}
	if (abs($by) < $e) {return $b;}
	if (abs($py) < $e) {return $p;}
    if ( ($ay * $by) > 0 ) {
		return wantarray?(undef,"Bad range: f($a) and f($b) have the same sign: $ay vs $by"):undef;
		}
	my $lastp;
	my $prep;
    for (1..$Max_Iter){
		if( $py * $ay < 0 ){
		    $b = $p;
		    $by = $py;
			}
		else{
		    $a = $p;
		    $ay = $py;
			}
		$lastp=$prep;
		$prep = $p;
		$p = ($a+$b)/2.0;
		$py = &$f($p);
		if( abs($py) < $e ){return $p;}
		
		if ($prep eq $p) {
			return wantarray?(undef,'bisection ran out of precision : maybe BigFloat would fix this, but usually not'):undef;
			#$p = Math::BigFloat->new($lastp);
			#$p->accuracy(40);
			#$py = &$f($p);
			#$py->accuracy(40);
			#$dobig = 1;
			}
		}
	return wantarray?(undef,'Maximum bisection iterations: possible bad solution'):undef;
    }

sub NewtonsMethod { 
	my $F = shift;
	my $bounds = shift;
	my $e = shift; # error threshold
	my $x = shift; #initial guess
	my $refok = (ref($x) || ref($bounds->[0]) || ref($bounds->[1]) || ref($e))?1:0;
	#my $h = @_?shift:($e/1000);#? quick guess - or could just specify a fixed tiny number - but should probably relate to epsilon and delta terms in calculus "good enough" definition
	my $h = $e/1000;#? quick guess - or could just specify a fixed tiny number - but should probably relate to epsilon and delta terms in calculus "good enough" definition
	if ($h < 0.000000000009) {$h=Math::BigFloat->new($h) if !ref($h);$x=Math::BigFloat->new($x) if !ref($x);}
	my $Fofx = (&{$F}($x));
	if (abs($Fofx) < $e) {return (ref($x) && !$refok)?$x->bstr:$x;} # maybe the guess was right on
	my $prex = ref($x)?$x->copy:$x;
	my $Fofxph=&{$F}($x + $h);

	for (1..$Max_Iter) {

		
		if ($Fofxph - $Fofx == 0 && !ref($x)) {
			# this might happen because we ran out of normal precision decimal places
			# before arriving at solution, so back up a step and use BigFloat numbers
			$h=Math::BigFloat->new($h) if !ref($h);
			$x=Math::BigFloat->new($prex);
			$Fofxph=&{$F}($x + $h);
			$Fofx=&{$F}($x);
			
			$x = ($x - ($Fofx/(($Fofxph - $Fofx)/$h))); # Newton's Iteration
			
			$Fofx=&{$F}($x);
			if (abs($Fofx) < $e) {return (ref($x) && !$refok)?$x->bstr:$x;}
			$Fofxph=&{$F}($x + $h);

			if ($Fofxph - $Fofx == 0) { # if we still have the problem, we're stuck
				#cluck 'divide by zero in Newton\'s Method'," x: $x x+h: ",($x+$h)," f(x): $Fofx Fofxph:$Fofxph";
				return wantarray?(undef,'divide by zero in Newton\'s Method'):undef;
				}
			}
		if ((defined($bounds->[0]) && $x < $bounds->[0]) || (defined($bounds->[1]) && $x > $bounds->[1])) { 
			#cluck 'out of bounds in Newton\'s Method';
			return wantarray?(undef,'out of bounds in Newton\'s Method'):undef;
			}
		if (defined($bounds->[1]) && ($x + $h) > $bounds->[1]) {$h = ($bounds->[1] - $x);if ($h < abs(0.000000000009)) {$h=Math::BigFloat->new($h);}}
		
		$prex=ref($x)?$x->copy:$x;
		$x = ($x - ($Fofx/(($Fofxph - $Fofx)/$h))); # Newton's Iteration
		
		$Fofx=&{$F}($x);
		if (abs($Fofx) < $e) {return (ref($x) && !$refok)?$x->bstr:$x;}
		$Fofxph=&{$F}($x + $h);
		}
	#cluck "too many iterations in Newton's Method";
	return wantarray? (undef,'too many iterations in Newton\'s Method'):undef;
	}


sub BrentsMethod {
	my $F=shift;
	my $bounds = shift;
	my $e=shift;
	my $guess = @_?shift:undef; #might use this for initial c or b, once I figure out which goes where. (c, I think)
	my $callername = @_?shift:'unknown'; 
	my $a=$bounds->[0];
	my $b=$bounds->[1];
	my $lowbound  = ($bounds->[0]<$bounds->[1])?$bounds->[0]:$bounds->[1];
	my $highbound = ($bounds->[0]>$bounds->[1])?$bounds->[0]:$bounds->[1];
	my $refok = 1;#(ref($a) || ref($b) || ref($e))?1:0;
	my $c=ref($a) ? $a->copy : $a;
	my $fofa=&{$F}($a);
	my $fofb=&{$F}($b);
	my $fofc=$fofa;
	my $fofd;
	my $m=($a + $b)/2;
	my $d = 0;
	my $lastd='';
	my $pred='';
	my $bigon=(ref($a) || ref($b) || ref($e))?1:0;

	if (abs($fofa) < $e || abs($fofa) eq $e) {return $a;}
	if (abs($fofb) < $e || abs($fofb) eq $e) {return $b;}
	if ($fofa * $fofb > 0) {return wantarray ? (undef,'f(a) and f(b) have same sign: '." f($a):$fofa vs  f($b):$fofb\n"):undef;}
	for (my $i=1;$i<=$Max_Iter;$i++) {

		if ($fofa != $fofc && $fofb != $fofc) { # inverse quadratic interpolation - Lagrange interpolating polynomial of degree 2
			$d = ($a * $fofb * $fofc)/(($fofa - $fofb) * ($fofa - $fofc)) + 
				 ($b * $fofa * $fofc)/(($fofb - $fofa) * ($fofb - $fofc)) + 
				 ($c * $fofa * $fofb)/(($fofc - $fofa) * ($fofc - $fofb));
			}
		else { # linear interpolation - secant method
			$d = $b - ( $fofb * ( ($b - $a)/($fofb - $fofa) ) );
			}
		if ( ($m < $b && ($d < $m || $d > $b)) ||
			 ($m > $b && ($d > $m || $d < $b)) ) { # bisection
			$d=$m;
			}

		$fofd = &{$F}($d);

		if (abs($fofd) < $e || abs($fofd) eq $e) {return (ref($d) && !$refok) ? $d->bstr:$d;}

    	if (ref($d) && $lastd eq $d->bstr) {
    		#turn up precision by factor of one hundred
    		print "upgrading precision : $d\n  function label is: $callername\n";
    		$d->precision( $d->precision() ? $d->precision() - 2 : -25 );
    		$d = Math::BigFloat->new($d->bstr . '01');
    		print " is now $d\n\n";
    		}

		#this retains the last point of opposite sign
		#which keeps root bracketed while shrinking size of bracket
		if (($fofb > 0 && $fofd < 0) || ($fofb < 0 && $fofd > 0)) {
			$a=$b;
			$fofa=$fofb;
			}
		$c=$b;
		$b=ref($d)?Math::BigFloat->new($d->bstr):$d;
		if ($a == $c) {
			$fofc=$fofa;
			}
		else {
			$fofc=$fofb;
			}
		$fofb=$fofd;
		$m = ($a + $b) / 2;
		$lastd=ref($d)?$d->bstr:"$d";
		}
	
	return wantarray?(undef,'too many iterations in Brent\'s method'):undef;
	}

sub BrentsMethod_NoBigFloatUpgrade {
	my $F=shift;
	my $bounds = shift;
	my $e=shift;
	my $guess = @_?shift:undef; #might use this for initial c or b, once I figure out which goes where. (c, I think)
	my $callername = @_?shift:'unknown'; 
	my $a=$bounds->[0];
	my $b=$bounds->[1];
	my $lowbound  = ($bounds->[0]<$bounds->[1])?$bounds->[0]:$bounds->[1];
	my $highbound = ($bounds->[0]>$bounds->[1])?$bounds->[0]:$bounds->[1];
	my $refok = 1;#(ref($a) || ref($b) || ref($e))?1:0;
	my $c=ref($a) ? $a->copy : $a;
	my $fofa=&{$F}($a);
	my $fofb=&{$F}($b);
	my $fofc=$fofa;
	my $fofd;
	my $m=($a + $b)/2;
	my $d = 0;
	my $lastd=$d;
	my $pred='';
	my $bigon=(ref($a) || ref($b) || ref($e))?1:0;
	if (abs($fofa) < $e || abs($fofa) eq $e) {return $a;}
	if (abs($fofb) < $e || abs($fofb) eq $e) {return $b;}
	if ($fofa * $fofb > 0) {return wantarray ? (undef,'f(a) and f(b) have same sign: '." f($a):$fofa vs  f($b):$fofb\n"):undef;}
	for (my $i=1;$i<=$Max_Iter;$i++) {
		if ($fofa != $fofc && $fofb != $fofc) { # inverse quadratic interpolation - Lagrange interpolating polynomial of degree 2
			$d = ($a * $fofb * $fofc)/(($fofa - $fofb) * ($fofa - $fofc)) + 
				 ($b * $fofa * $fofc)/(($fofb - $fofa) * ($fofb - $fofc)) + 
				 ($c * $fofa * $fofb)/(($fofc - $fofa) * ($fofc - $fofb));
			}
		else { # linear interpolation - secant method
			$d = $b - ( $fofb * ( ($b - $a)/($fofb - $fofa) ) );
			}
		if ( ($m < $b && ($d < $m || $d > $b)) ||
			 ($m > $b && ($d > $m || $d < $b)) ) { # bisection
			$d=ref($m)?$m->copy:$m;
			}
		$fofd = &{$F}($d);
		if (abs($fofd) < $e || abs($fofd) eq $e) {return (ref($d) && !$refok) ? $d->bstr:$d;}
		if ($lastd eq $d) {return wantarray ? (undef,"d and last d are equal: $lastd eq $d"):undef;}
		if ($fofb * $fofd < 0) {$a=ref($b)?$b->copy:$b;}
		$c=ref($b)?$b->copy:$b;
		$b=ref($d)?$d->copy:$d;
		$fofa=&{$F}($a);
		$fofb=&{$F}($b);
		if ($a == $c) {$fofc=$fofa;}
		else {$fofc=&{$F}($c);}
		$m = ($a + $b) / 2;
		$lastd=$d;
		}
	return wantarray?(undef,'too many iterations in Brent\'s method'):undef;
	}

#sub popplot {
#	my $ys = shift;
#	my $xs = @_?shift:undef;
#	my $ti = @_?shift:'popplot';
#	my $mw = MainWindow->new;
#	my $cp = $mw->LineGraph(-width=>500, -height=>500, -background => 'snow')->grid;
#	my $ds;
#	my $ds2;
#	if (defined $xs) {$ds = LineGraphDataset->new(-yData=>$ys,-name=>$ti,-xData=>$xs);}
#	else {$ds = LineGraphDataset->new(-yData=>$ys,-name=>$ti);}
#	$cp->plot(-dataset=>$ds);
#	MainLoop;
#	}
1;
