package Math::QuadraticFormula;
use vars qw($VERSION);
require Exporter;
@ISA = qw(Exporter);
@EXPORT    = qw(quadraticformula);
#my $epsilon=0.00000000000001;
sub quadraticformula
	{
	my ($a, $b, $c);
	($a, $b, $c) = @_;
	$realonly=@_?shift:0;
	my $im = '';
#okay, first problem: how do we know wether 0.000000001 is close enough to zero to be considered zero?
# and then, if $a is close to zero, why use it as denominator? what was I thinking?
#	if (abs($a) < 0.000000001) {return (-$b/$a - $c);} # just a line
#what you probably should ask is, is $a less thanpotential round off error (or maybe roundoff error / 2)
#but how do we know what round off error is? 
#	return (0, -$b/$a) if (abs($c) < $epsilon);     # because it degrades to x(ax+b)

	return (0, -$b/$a) if (abs($c) eq 0);     # because it degrades to x(ax+b)
	return (-$c/$b)    if (abs($a) eq 0);     # because it degrades to bx+c

	my $dis = $b*$b - $a * 4 * $c;                  # discriminant
	my $a2  = 2*$a;                                 # 2a
	my $nb2a;

	if ($b == 0) {$nb2a=0;}
	else {$nb2a = -1 * $b/$a2;}                          # -b/2a
#	if ($dis - 0 < 0.00000000000001) {return $nb2a;}# one root
	if ($dis < 0 && !$realonly) {                   # imaginary roots
		my $imaginary;
		if (ref($a) || ref($b) || ref($c)) { $imaginary = (bigsqrt($a * 4 * $c - $b*$b)/$a2) .'i'; }
		else  { $imaginary = (sqrt($a * 4 * $c - $b*$b)/$a2) .'i'; }
		return ($nb2a.' + '.$imaginary,$nb2a.' - '.$imaginary);
		}
	elsif ($dis >= 0) {                                          # one (double) or two real roots
		my $rightside;
		if ($a2==0) {$rightside=0;}
		else {
			if (ref($dis)) {$rightside = bigsqrt($dis)/$a2;}
			else  {$rightside = sqrt($dis)/$a2;}
			}
		return ($nb2a + $rightside,$nb2a - $rightside);
		}
	else {return;}
	}
our $BigFloatOneHalf = Math::BigFloat->new('0.5');
our $BigFloatTen     = Math::BigFloat->new('10');
sub bigsqrt {
	#Wikipedia says:
	#sqrt(x) = 10**(1/2 * log_10(x))
    # putting precision on the end ( the ",25" ) made this a bit faster
    # BigFloat docs suggest default "div_scale" is 40
    # So I'm limiting the advantage of BigFloats here by only getting maybe 10 extra decimal places instead of 35
    return $BigFloatTen->copy()->bpow($BigFloatOneHalf->copy()->bmul($_[0]->copy()->blog(10)),25);
	}
1;