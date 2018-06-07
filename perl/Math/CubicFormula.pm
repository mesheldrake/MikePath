package Math::CubicFormula;
use vars qw($VERSION);
require Exporter;
@ISA = qw(Exporter);
@EXPORT    = qw(cubicformula);
use Math::Big;
#use Math::BigFloat lib => 'GMP';
use Math::BigFloat;
use Time::HiRes qw(tv_interval gettimeofday);
use strict;

my $pi = new Math::BigFloat '3.141592653589793238462643383279502884197169399375105820974944592308';
my $piovertwo = $pi->copy()->bdiv(2);
my $twopi = $pi->copy()->bmul(2);
#my $pi = 4 * CORE::atan2(1, 1);
my $bigpi=0;
my $littlepi=3.141592653589793238462643383279502884197169399375105820974944592308;
#sub pi {($bigpi)?$pi:'3.14159265358979';}; # just put this login inline instead - function call for that seems excessive
#print "PIE (no):", (pi) ,"\n";

# test case:
#print '   ',join(", ",cubicformula(-11,49,-75)),"\nvs 3, 4+3i, 4-3i\n";


sub cubicformula { #Stephen R. Schmitt http://home.att.net/~srschmitt/script_exact_cubic.html
                   #translated from his javascript
    my $A = shift;
    my $B = shift;
    my $C = shift;
    $bigpi = (ref($A) || ref($B) || ref($C))?1:0;
    my $onlyreal = @_?shift:0;
    my @Im = &cubic($A, $B, $C);
    if ($Im[0].'a' eq '0.0a') { #trying to force string interpretation # real roots
        return @Im[1..3];
        }
    else {                                          # real and complex pair
        if ($onlyreal) {
            return $Im[1];
            } 
        else {
            return ($Im[1],$Im[2].' + '.$Im[0].'i',$Im[3].' - '.$Im[0].'i');
            }
        }
    }
# compute real or complex roots of cubic polynomial
sub cubic {
    my $A = shift;
    my $B = shift;
    my $C = shift;
    my ($Q, $R, $D, $S, $T, $Im, $X1, $X2, $X3);

    $Q = (3*$B - $A**2)/9;
    $R = (9*$A*$B - 27*$C - 2*$A**3)/54;
    $D = $Q**3 + $R**2;                             # polynomial discriminant
    if ($D > 0 || $D eq 0) {                        # complex or duplicate roots
        my $sqrtD=(ref($D)?bigsqrt($D):sqrt($D));
        my $preS=$R + $sqrtD;
        my $preT=$R - $sqrtD;
        $S = (($preS < 0)?-1:1) * (abs($preS)**(1/3));
        $T = (($preT < 0)?-1:1) * (abs($preT)**(1/3));
        $X1 = (-1 * $A)/3 + ($S + $T);              # real root
        $X2 = (-1 * $A)/3 - ($S + $T)/2;            # real part of complex root
        $X3 = $X2;                                  # real part of complex root
        if ($D eq 0) {$Im='0.0';}                   # $D==0 case, duplicate real roots
        else {$Im = abs(sqrt(3)*($S - $T)/2);}      # complex part of root pair
        }
    else {                                          # distinct real roots
        #my $th = acos($R/sqrt(-1 * ($Q**3)));

		my $toAcos;
		my $sqrtofnegQcubed = sqrt( -1.0 * ($Q**3) );

		if (ref($Q) || abs($R - $sqrtofnegQcubed) < 0.000000000001) { # second thing is to be careful near +/-1 , limits of arctan function that gets used soon
			if (!ref($Q)) {$Q=Math::BigFloat->new(''.$Q);} #needed if new second part of that if is true and first isn't 
			my $presqrtofnegQcubed=$Q->copy();
			$presqrtofnegQcubed->bpow(3);
			$presqrtofnegQcubed->bmul(-1.0);
			#you can't trust BigFloat's root or sqrt functions
			$sqrtofnegQcubed = bigsqrt($presqrtofnegQcubed);
			$R = Math::BigFloat->new(''.$R); #upgrade so / operator overload works with R on left setting Big mode
			}
		if (ref($Q)) {
			$toAcos = $R / $sqrtofnegQcubed;
			}
		elsif (ref($R))  {
			$toAcos = $R->copy()->bdiv($sqrtofnegQcubed);
			}
		else  {
			$toAcos = $R / $sqrtofnegQcubed;
			}


		my $th;

		if (ref($toAcos)) {
			my $noom=$toAcos->copy();
            if ($noom eq 'NaN') {die "noom is NaN after big copy from toAcos\n";}
			$noom->bpow(2)->bmul(-1.0)->badd(1.0);
            if ($noom eq 'NaN') {die "noom is NaN after series of pow, mul, add operations\n";}

            if ($noom < 0) {
                # snap neg to zero - not sure that's good, but maybe - 
                # guessing this should not ever go negative
                $noom = Math::BigFloat->bzero;
                }
            my $newnoom;
            if ($noom <= 0) {$newnoom=Math::BigFloat->bzero;}
            else {$newnoom=bigsqrt($noom);}
            if ($newnoom eq 'NaN') {die "noom is NaN after bigsqrt. before, noom was: ",$noom->bstr,"\n";}
            else {$noom = $newnoom;}
			# This arctan is not the same as atan2 or even atan - it's input is 
			# limited to abs(x)<=1. Math::Big::arctan uses a slower-to-converge
			# Taylor series that is prone to not converging near 1, so I made my
			# own arctan based on some faster Euler series.
			my $abscmp=$toAcos->bacmp($noom);
			if ($abscmp < 0 || $abscmp eq 0) {
				my $quo=$toAcos->copy()->bdiv($noom);
                if ($quo eq 'NaN') {die "pre arctan_Euler call 1: have a NaN derived from toAcos/noom : ",$toAcos->bstr," / ",$noom->bstr,"\n"}
				$th=arctan_Euler($quo,25);
				$th->bmul(-1)->badd($piovertwo);
				}
			else {
				my $quo=$noom->copy()->bdiv($toAcos);
                if ($quo eq 'NaN') {die "pre arctan_Euler call 2: have a NaN derived from noom/toAcos : ",$noom->bstr," / ",$toAcos->bstr,"\n"}
				$th=arctan_Euler($quo,25);
				}
			$th = &atanfix($th,$toAcos,$noom);
			}
		else {$th=atan2(sqrt(1 - $toAcos * $toAcos),$toAcos);}
        if ($bigpi || ref($th) || ref($A) || ref($Q) || ref($R)) {

            if (ref($Q)) {
				my $twotimessqrtofnegQ=$Q->copy()->bmul(-1.0);
				$twotimessqrtofnegQ = bigsqrt($twotimessqrtofnegQ);
				$twotimessqrtofnegQ->bmul(2.0);
				$X1 = $twotimessqrtofnegQ *       Math::Big::cos($th/3         ,40) - $A/3;
            	$X2 = $twotimessqrtofnegQ *       Math::Big::cos(($th + 2*($bigpi?$pi:$littlepi))/3,40) - $A/3;
            	$X3 = $twotimessqrtofnegQ *       Math::Big::cos(($th + 4*($bigpi?$pi:$littlepi))/3,40) - $A/3;
				}
			else {
				my $twotimessqrtofnegQ=2*sqrt(-1 * $Q);
				$X1 = $twotimessqrtofnegQ *       Math::Big::cos($th/3         ,40) - $A/3;
            	$X2 = $twotimessqrtofnegQ *       Math::Big::cos(($th + 2*($bigpi?$pi:$littlepi))/3,40) - $A/3;
            	$X3 = $twotimessqrtofnegQ *       Math::Big::cos(($th + 4*($bigpi?$pi:$littlepi))/3,40) - $A/3;
				}
			}
		else {
            my $twotimessqrtofnegQ=2*sqrt(-1 * $Q);
			$X1 = $twotimessqrtofnegQ * CORE::cos($th/3) - $A/3;
            $X2 = $twotimessqrtofnegQ * CORE::cos(($th + 2*($bigpi?$pi:$littlepi))/3) - $A/3;
            $X3 = $twotimessqrtofnegQ * CORE::cos(($th + 4*($bigpi?$pi:$littlepi))/3) - $A/3;

#my $teststr='0.0002657487382';
#if (0 && ($X1=~/^$teststr/ || $X2=~/^$teststr/ || $X3=~/^$teststr/)) {
#warn "[$X1,$X2,$X3,$twotimessqrtofnegQ,th $th,R $R,Q $Q, toAcos $toAcos, $bigpi]\n";
#warn "warn in old";
#}

			}

		$Im = '0.0';
	    }

    return ($Im,$X1,$X2,$X3);                                  # 0.0 if real roots
	}

sub bigsqrt {
	#Wikipedia says:
	#sqrt(x) = 10**(1/2 * log_10(x))
	return Math::BigFloat->new(10)->bpow(Math::BigFloat->new('0.5')->bmul($_[0]->copy()->blog(10,25)),25)
	}
sub atanfix {
	my $angle=shift;
	my $x=shift;
	my $y=shift;
	if    ($x<0 && $y>0 && $angle<0) {if (ref($angle)) {$angle->badd($pi,25);} else {$angle+=$littlepi};}
	elsif ($x<0 && $y<0 && $angle>0) {if (ref($angle)) {$angle->bsub($pi,25);} else {$angle-=$littlepi;}}
	return $angle;
	}
sub arctan_Euler {
	my $x = shift || 0;
	if ($x>1) {die "out of bounds x to arctan_Euler: $x\n";}
	my $d = abs(shift || 42);
	$d += 1;
	$x = Math::BigFloat->new($x) if ref($x) ne 'Math::BigFloat';
	my $xsq=$x->copy()->bpow(2);
	my $xsqPone=$xsq->copy()->badd(1);
	my $y = $xsq->copy()->bdiv($xsqPone);    # x^2/(1+x^2);
	my $arctan = Math::BigFloat->bone();
	my $overy = $y->copy();
	my $multop=2;
	my $cot=Math::BigFloat->new($multop);
	my $cob=Math::BigInt->new($multop + 1);
	my $co = $cot/$cob;#should come out as BigFloat because numerator is of that type
	while ($arctan->bstr ne $arctan->badd($overy * $co,$d)->bstr ) { #copy,mul  # this is the most expensive line, I think
		$overy->bmul($y);                                            #mul
		$multop+=2;
		$co = ($cot->bmul($multop)) / ($cob->bmul($multop + 1));      #mul,mul,copy,div
		}
	$x->bdiv($xsqPone);
	$arctan->bmul($x);
	return $arctan->round($d-1);
	}
1;