function numsrt(x,y) {return x-y}
if (!Array.prototype.map)
{
  Array.prototype.map = function(fun /*, thisp*/)
  {
    var len = this.length;
    if (typeof fun != "function")
      throw new TypeError();

    var res = new Array(len);
    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in this)
        res[i] = fun.call(thisp, this[i], i, this);
    }
    return res;
  };
}
function angle_reduce(ang) { // # Copied from CAD::Calc
	var angbef = ang;
	while(ang > Math.PI) {
		ang -= 2*Math.PI;
		}
	while(ang <= -Math.PI) {
		ang += 2*Math.PI;
		}
	return ang;
	}

function mergePaths(path1,path2) {
	var segs = new Array();
	segs[0]=path1.pathSegmentSpecs[0];
	for (var i=1;i<path1.pathSegmentSpecs.length;i++) {segs.push(path1.pathSegmentSpecs[i]);}
	for (var i=1;i<path2.pathSegmentSpecs.length;i++) {segs.push(path2.pathSegmentSpecs[i]);}
	var ret = new MikePath(segs.join(''),path1.resolution,path1.precision);
	//foreach (@_) {undef($_);}
	return ret;
	}
function MikePath() {
	if (new String(arguments[0]).match(/^((file|http|ftp)\:.*)/)) {	
		//$_=get($1);die "Couldn't get file: $1" unless defined $_;
		//s/^#.*\r?\n//g;s/\r?\n//g;
		//push(@_,$_);
		
		this.req = new XMLHttpRequest();
		this.req.open('GET', RegExp.$1, false);
		this.req.send(null);
		this.pathspec=this.req.responseText;
		}
	else {
		this.pathspec = arguments[0];
		}
	this.resolution = arguments[1];
	this.precision = (arguments.length>2)?arguments[2]:this.resolution/1000;
	this.constructSegments(this.pathspec);
	}
MikePath.prototype.constructSegments = function() {
	this.pathspec = arguments[0];
	this.pathSegmentSpecs = new Array();

    // Strip spaces around path seg letters, since we split on spaces 
    // (or commas) later to extract coordinates for each pathseg string,
    // and leading spaces, especially, will mess that up.
    this.pathspec = this.pathspec.replace(/([MmZzLlHhVvCcSsQqTtAa]) +/g,function(m,p1) {return p1;});
    this.pathspec = this.pathspec.replace(/ +([MmZzLlHhVvCcSsQqTtAa])/g,function(m,p1) {return p1;});

	this.pathSegmentSpecs = this.pathspec.match(/[MmZzLlHhVvCcSsQqTtAa][0-9, \-\.e]*/g);
	for (var i=0;i<this.pathSegmentSpecs.length;i++) {
        this.pathSegmentSpecs[i] = this.pathSegmentSpecs[i].replace(/^ +/,'');
        this.pathSegmentSpecs[i] = this.pathSegmentSpecs[i].replace(/ +$/,'');
        }
	this.pathSegments=new Array();
	var lastM=this.pathSegmentSpecs[0];
	var lastSegSpec=this.pathSegmentSpecs[0];
	var throwstr='';
	var otherthrowstr='';
	var anotherthrowstr='';
	for (var i=1;i<this.pathSegmentSpecs.length;i++) {
		var thisSegSpec=this.pathSegmentSpecs[i];
		if (thisSegSpec.indexOf('M') == 0) {lastM=this.pathSegmentSpecs[i];} //#so we can start to be smart about paths with subpaths (multiple Ms)
		if (thisSegSpec.indexOf('Z') == 0 || thisSegSpec.indexOf('z') == 0) {																		thisSegSpec='Z' + lastM.substring(1);throwstr+=""+lastM.substring(1)+"\n";} //# so we can treat it as a LineSegment, and start to be smart about paths with subpaths (multiple Ms). Ee should flag it as special case somehow, but we don't yet
		if ((lastSegSpec.indexOf('Z') == 0 || lastSegSpec.indexOf('z') == 0) && thisSegSpec.indexOf('M') != 0) {lastSegSpec=lastM;otherthrowstr += "["+lastM+"]\n";} //#per SVG spec - if no new M following Z, but something else, that something else is from last M
		this.pathSegments[i - 1] = this.constructSegment(thisSegSpec,lastSegSpec);
		if (! this.pathSegments[i - 1]) {anotherthrowstr+=i + " : " + this.pathSegmentSpecs[i] + "\n path:\n"+this.pathspec+"\n\n";};
		lastSegSpec=thisSegSpec;
		}
	
	for (var i=0;i<this.pathSegments.length;i++) {
		if (! this.pathSegments[i]) {
			//alert("problem i:"+i+" pathspec: "+this.pathspec);
			var er="eleftee: "+window.lefty+"\nproblem i:"+i+"\nanotherthrowstr:\n"+anotherthrowstr+" throwstr:\n"+throwstr+" otherthrowstr:\n"+otherthrowstr+"\npathspec: "+this.pathspec.split('L').join('\nL');
			//displaytext("eleftee: "+window.lefty+"\nproblem i:"+i+"\nanotherthrowstr:\n"+anotherthrowstr+" throwstr:\n"+throwstr+" otherthrowstr:\n"+otherthrowstr+"\npathspec: "+this.pathspec.split('L').join('\nL'));
			console.log(er);
			}
		}
	
	var minxs=new Array(); for (var i=0;i<this.pathSegments.length;i++) {minxs[minxs.length]=this.pathSegments[i].minx;} minxs.sort(numsrt);
	this.minx = minxs[0];
	var maxxs=new Array(); for (var i=0;i<this.pathSegments.length;i++) {maxxs[maxxs.length]=this.pathSegments[i].maxx;} maxxs.sort(numsrt).reverse();
	this.maxx = maxxs[0];
	var minys=new Array(); for (var i=0;i<this.pathSegments.length;i++) {minys[minys.length]=this.pathSegments[i].miny;} minys.sort(numsrt);
	this.miny = minys[0];
	var maxys=new Array(); for (var i=0;i<this.pathSegments.length;i++) {maxys[maxys.length]=this.pathSegments[i].maxy;} maxys.sort(numsrt).reverse();
	this.maxy = maxys[0];
    //#	print "minx:$self->{minx},maxx:$self->{maxx},miny:$self->{miny},maxy$self->{maxy}\n";
    if (window.noww) {console.log("minx:"+this.minx+"maxx:"+this.maxx+"miny:"+this.miny+"maxy:"+this.maxy);}
	}
MikePath.prototype.parameterize = function() {
	var newlength=0;
	var toff=0;
	for (var i=0;i<this.pathSegments.length;i++) {this.pathSegments[i].length=this.pathSegments[i].getLength();newlength+=this.pathSegments[i].length;}
	this.length = newlength;
	this.pathThetaToCompThetaRanges=new Array();
	for (var i=0;i<this.pathSegments.length;i++) {
		this.pathThetaToCompThetaRanges.push(new Array(toff,toff + (this.pathSegments[i].length/this.length)));
		toff += (this.pathSegments[i].length/this.length);
		//#now the indexes of those ranges should match the indexes (indeces) of the components
		}
	this.pathThetaToCompThetaRanges[this.pathThetaToCompThetaRanges.length - 1][1]=1;
	}
MikePath.prototype.getSegsInRange = function() {
	var point= arguments[0];
	var ret=new Array();
	for (var i=0;i<this.pathSegments.length;i++) {
		var test = this.pathSegments[i].inRange(point);
		if (test[0] || test[1]) {ret[ret.length]=this.pathSegments[i];}
		}
	return ret;
	}
MikePath.prototype.precision = function() {
	var p=parseFloat(arguments[0]);
	if (typeof(p) == "number" && !isNaN(p)) {
		this.precision=p;
		this.thetaprecision = this.precision/this.xmax;
		for (var i=0;i<this.pathSegments.length;i++) {
			this.pathSegments[i].precision=this.precision;
			this.pathSegments[i].thetaprecision=this.thetaprecision;
			}
		}
	return this.precision;
	}

MikePath.prototype.f = function() {
	var x=arguments[0];
	var res=new Array();
	var sir=this.getSegsInRange(new Array(x,false));
	for (var i=0;i<sir.length;i++) {
		var segrets=sir[i].f(x);
		if (segrets.length) {for (var j=0;j<segrets.length;j++) {res.push(segrets[j]);}}
		else if (segrets || segrets == 0) {
			res.push(segrets);
			}
		}
	var dupseive = new Object();
	var resret=new Array();
	for (var i=0;i<res.length;i++) {
		if (!dupseive[res[i]]) {
			dupseive[res[i]]=true;
			resret[resret.length]=res[i];
			}
		}
	return (arguments[1])? resret : resret[0];
	}
MikePath.prototype.F = function() {
	var y=arguments[0];
	//var scalefactor = (arguments[1])?arguments[1]:1;// line seems misguided, obsolete
	var res = new Array();
	var sir = this.getSegsInRange(new Array(false,y));
	for (var i=0;i<sir.length;i++) {
		var segrets=sir[i].F(y);
		if (segrets.length) {for (var j=0;j<segrets.length;j++) {res.push(segrets[j]);}}
		else if (segrets || segrets == 0) {
			res.push(segrets);
			}
		}
	var dupseive = new Object();
	var resret=new Array();
	for (var i=0;i<res.length;i++) {
		if (!dupseive[res[i]]) {dupseive[res[i]]=true;resret[resret.length]=res[i];}
		}
	return (arguments[1])? resret : resret[0];
	}
MikePath.prototype.point = function() {
	var theta = arguments[0];
	if (!this.pathThetaToCompThetaRanges) {this.parameterize();} //#delay this until you need it
	var seg=this.getSegThetaIndexAtPathTheta(theta);
	if (!seg) {throw " teheta: "+theta}
	//#print "path->point($theta) find seg?: $seg[0], $seg[1], $seg[2]\n";
	return seg[0].point(seg[1]);
	}
MikePath.prototype.getSegThetaIndexAtPathTheta = function() {
//# how do you map segment's non-uniform theta spacing to your path-wide pseudo-theta indexing?
//# here I'm tring to find the segments native theta that correspondes to a given fraction of it's length
//# Expensive, but should be doable. Make it work, then decide if it's too expensive.
	var theta = parseFloat(arguments[0]);
	if (!this.pathThetaToCompThetaRanges) {this.parameterize();} //#delay this until you need it
	for (var i=0;i < this.pathThetaToCompThetaRanges.length;i++) {
		//#print "test:  (($self->{pathThetaToCompThetaRanges}->[$i]->[0] < $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[0] eq $theta)  &&  ($self->{pathThetaToCompThetaRanges}->[$i]->[1] > $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[1] eq $theta)) ? ",((($self->{pathThetaToCompThetaRanges}->[$i]->[0] < $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[0] eq $theta) && ($self->{pathThetaToCompThetaRanges}->[$i]->[1] > $theta || $self->{pathThetaToCompThetaRanges}->[$i]->[1] eq $theta))?'yes':'no'),"\n";
		if (
			(this.pathThetaToCompThetaRanges[i][0] < theta || this.pathThetaToCompThetaRanges[i][0] == theta) 
			&& 
			(this.pathThetaToCompThetaRanges[i][1] > theta || this.pathThetaToCompThetaRanges[i][1] == theta)
			) {
			var segoffsetlengthfraction = (theta - this.pathThetaToCompThetaRanges[i][0]) / (this.pathThetaToCompThetaRanges[i][1] - this.pathThetaToCompThetaRanges[i][0]);
			var segtheta=segoffsetlengthfraction; //#might that be good enough? w/o worrying about real length issues?
			return new Array(this.pathSegments[i],segtheta,i);
			}
		}
	}
MikePath.prototype.getPathThetaAtSegTheta = function() {
	var segindex = parseInt(arguments[0]);
	var segtheta = parseFloat(arguments[1]);
	if (!this.pathThetaToCompThetaRanges) {this.parameterize();} //#delay this until you need it
	var theta = ( segtheta * (this.pathThetaToCompThetaRanges[segindex][1] - this.pathThetaToCompThetaRanges[segindex][0]) ) + this.pathThetaToCompThetaRanges[segindex][0];
	//#print "getPathThetaAtSegTheta:\n$theta = ( $segtheta * (",$self->{pathThetaToCompThetaRanges}->[$segindex]->[1]," - ",$self->{pathThetaToCompThetaRanges}->[$segindex]->[0],") ) + ",$self->{pathThetaToCompThetaRanges}->[$segindex]->[0],";\n";
	return theta;
	}
MikePath.prototype.curve = function() {
	var cnt = (arguments[0])?arguments[0]:20;
	var ret=new Array;
	
	var inc = 1/cnt;
	var roughlengths=new Array();
	var roughlength=0;
	for (var i=0;i<this.pathSegments.length;i++) {
		var seg=this.pathSegments[i];
		roughlengths[i]=Math.sqrt(Math.pow(this.pathSegments[i].p2[1] - this.pathSegments[i].p1[1],2) + Math.pow(this.pathSegments[i].p2[0] - this.pathSegments[i].p1[0],2));
		roughlength += roughlengths[i];
		}
	ret.push(this.pathSegments[0].point(0));
	for (var i=0;i<this.pathSegments.length;i++) {
		var cntshare = cnt * (roughlengths[i]/roughlength) * 1;
		var inc = 1/cntshare;
		for (var j=inc;j<1;j+=inc) {
			ret.push(this.pathSegments[i].point(j));
			}
		ret.push(this.pathSegments[i].point(1));
		}
	return ret;
	}
MikePath.prototype.dimensionalCurve = function(step,startstep,endstep,segrange) {

    // originally developed here in javascript MikePath
    // ported to perl MikePath and improved over there
    // improvements ported back here, hopefully
    // and keep doing that - keep in sync
    
    if (!startstep) {startstep=0;} // if undefined, set to zero, so we can use it in math
    // if endstep is undefined, it should stay that way, to differentiate between that and defined but zero
	var segrange = segrange?segrange:[0,this.pathSegments.length - 1];
	var ret=new Array; //# list of points like [x,y,normal,theta] (theta is (normalized, i.e. fraction of distance) theta on whole path, not from segs)
	var endpt;
	var segslength=0;
	for (var i=segrange[0];i<=segrange[1];i++) {
        // default theta divisor (sample count) is 1000,
        // but that's not enough it looks like at board scale -
        // determined while working on rail piece design
	    segslength+=this.pathSegments[i].getLength(10000);
	    }
	var lengthsum=0;
	var lengthfraction=lengthsum/segslength;
	var stepfunc=(typeof(step)=='function')?step:new Function("return "+step+";");

	var stepval=stepfunc(lengthfraction);

	// See TODO in perl version re: if passed-in step was a function, which
	// probably needs to complicate this firststep calculation
	var firststep=startstep?startstep*stepval:0;

	var laststep; //# undef is significant, because "0" needs to be a true last step request - actually we don't make that test anywhere below though

    // Check if the whole path is too short to subdivide by the requested dimension.
	if (segslength < firststep) {console.log("path too short ("+segslength+") to divide by first step "+firststep+"\n");return [];}

    //# If the step size is fixed, and we want to hit a specified end point,
    //# adjust step size to be smaller than requested,
    //# to hit that endpoint in a whole number of steps.
	if (typeof(step)!='function' && !isNaN(endstep)) { // i.e., defined(endstep)==true
		laststep = (1 - endstep) * stepval;
		var fitlength=segslength - (firststep + (stepval - laststep));
		var d = fitlength/step;
        if (d<1) {// # Path too short to support both the requested first step offset and last step offset
            stepfunc = new Function("return " + step); //# so this will overshoot on the first step, and result will just have the first step point
            }
        else {

            // a*adjstd + b*adjstd + c*adjstd = segslength, c is number of steps we'll end up with
            // (a + b + c) * adjstd = segslength
            // (AB + c) * adjstd = segslength
            // a and b are known
            // c can be one more than what you get when you figure this with unadjusted step size
            // call that C
            // so,
            // adjstd = segslength/(A+B+C)
            // yep. looks reasonable.

            var C = (Math.floor(d) + 1);
            var ABC = ( startstep + endstep + C );
            var adjustedstep = segslength / ABC;
            stepfunc = new Function("return "+adjustedstep+";");
            // console.log('adjusted: ' + step + ' to ' + adjustedstep);
            stepval = stepfunc(lengthfraction);
            firststep = startstep * adjustedstep;
            laststep = (1 - endstep) * adjustedstep; // laststep isn't used, is it?

            }
		}

    //# Otherwise, if the step size is fixed, and no end point offset is specified
    //# use exactly requested step size, and don't care where the last point lands,
    //# as long as it's less than stepsize from end of last segment.

    //# TODO: (this todo is duplicated in both perl and javascript versions)
    //# variable (assumed) step size, so need to smart-figure what first step is, and for last step
    //# that depends both on first step and step function, so weirder to figure.
    //# maybe analogous to fixed step case, you can find a tiny number to subtract from each input to the eval of the step function
    //# to shrink the resulting steps (unless the function's weird and that makes them grow? detect that?)
    //# If you pull that off, your rail designer will work like you want, more, sorta.
    
    //# The key feature of that would be that you could do a 50% step on that first step
    //# and then a mirror version of the path, mirrored through start point, would
    //# have a "natural" looking space between it's first point and the orig first point.
    //# "Natural" in view of the way the step function is varying steps in that region.

	//#elsif (ref($step) && (defined $endstep || defined $endstep)) {}

	var zeropt=this.pathSegments[segrange[0]].point(0);
	zeropt.push(this.pathSegments[segrange[0]].angleNormal_byTheta(0));
	zeropt.push(lengthfraction);
	if (stepval == firststep || ( isNaN(startstep) || startstep == '0' ) ) {ret.push(zeropt);}

	var endpt; //# set to each seg endpoint in for loop,
	           //# and we might use the last one, after the for loop

	for (var i=segrange[0];i<=segrange[1];i++) {
		var prevtheta=0;
		var firstpt=this.pathSegments[i].point(prevtheta);
		firstpt.push(this.pathSegments[i].angleNormal_byTheta(prevtheta),lengthfraction);
		thispointandtheta=new Array(firstpt,0);
		var prevptnth=thispointandtheta;
		endpt=this.pathSegments[i].point(1);
		endpt.push(this.pathSegments[i].angleNormal_byTheta(1),1);

		stepval=stepfunc(lengthfraction);
		
        var dist_to_end = Math.sqrt(
                       Math.pow(endpt[1] - thispointandtheta[0][1], 2)
                     + Math.pow(endpt[0] - thispointandtheta[0][0], 2)
        );

        var first_while_loop = 1;
		
		while ( dist_to_end > stepval ) {
		    
			var thisstep=stepval;
			if (firststep) {thisstep=firststep;firststep=false;}
			prevptnth=thispointandtheta;	
			thispointandtheta=this.pathSegments[i].dimensionalStepFromTheta(thisstep,prevtheta);
			lengthsum+=Math.sqrt(Math.pow(thispointandtheta[0][0]-prevptnth[0][0],2) + Math.pow(thispointandtheta[0][1]-prevptnth[0][1],2));
			lengthfraction=lengthsum/segslength;;
			stepval=stepfunc(lengthfraction);
			thispointandtheta[0].push(this.pathSegments[i].angleNormal_byTheta(thispointandtheta[1]),lengthfraction);
			ret.push(thispointandtheta[0]);

            //# avoid infinite loop when theta isn't changing
            if (prevtheta == thispointandtheta[1] 
                //# A same-theta result of 0 can be valid on the first run through
                //# this while loop, if our previous point left us very very close
                //# to where this step (which would be a $firststep) being right 
                //# on this new seg's start point. So that's what this test is for:
                && !(prevtheta == 0 && first_while_loop)
               ) {
                console.log("hit LAST to avoid infinite loop when theta isn't changing\nprob should be die?\n");
                //last;
                break;
                }

			prevtheta=thispointandtheta[1];
            dist_to_end = Math.sqrt(
                           Math.pow(endpt[1] - thispointandtheta[0][1], 2)
                         + Math.pow(endpt[0] - thispointandtheta[0][0], 2)
            );

            first_while_loop = 0;
			}

        //# TODO: actually hitting every segment endpoint might be a usful
        //#       option to stick right here. You _don't_ want that for what
        //#       you're using this for now, mostly, like getting nicely spaced
        //#       locations for radial rail pieces. But it would be good for
        //#       other things where you need to capture inflection points where
        //#       segments meet along a path.

		//# While loop ended because next step would have stepped beyond
		//# end of current segment. Now set first step for next segment,
		//# accounting for whatever partial step distance was taken up by
		//# the end of this segment.
		firststep=stepval - dist_to_end;
		
		}

    //# The while loop should _not_ get the last step in this case.
    //# (though rarely it might? giving duplicate to this point?)
    if (!isNaN(endstep) && endstep == 0) {
        ret.push(endpt);
        }

	return ret;
	}

// TODO: this is different than Perl version. Which is newer? Which is better?
MikePath.prototype.getLength = function() {
	var stepsize=arguments.length>0?arguments[0]:Math.sqrt(Math.pow(this.maxx - this.minx,2) + Math.pow(this.maxy - this.miny,2))/1000;
	if (arguments.length>0 || !this.length) {
		segrange=new Array(0,this.pathSegments.length - 1);
		var pts=this.dimensionalCurve(stepsize);
		this.length=0;
		for (var i=1;i<pts.length;i++) {this.length+=Math.sqrt(Math.pow(pts[i][0] - pts[i-1][0],2) + Math.pow(pts[i][1] - pts[i-1][1],2));}
		}
	return this.length;
	}
MikePath.prototype.slopeTangent = function() {
	var res = new Array;
	var sir = this.getSegsInRange(new Array(arguments[0],arguments[1]));
	for (var i=0;i<sir.length;i++) {
		res.push(sir[i].slopeTangent(arguments[0],arguments[1]));
		}
	return (arguments[3])?res:res[0];//hmm, this is instead of PERL's wantarray
	}
MikePath.prototype.slopeNormal = function()  {
	var res = new Array();
	var sir = this.getSegsInRange(new Array(arguments[0],arguments[1]));
	for (var i=0;i<sir.length;i++) {
		res.push(sir[i].slopeNormal(arguments[0],arguments[1]));
		}
	return (arguments[3])?res:res[0];//hmm, this is instead of PERL's wantarray 
	}
MikePath.prototype.slopeTangent_byTheta = function() {
	var seg=this.getSegThetaIndexAtPathTheta(arguments[0]);
	return seg[0].slopeTangent_byTheta(seg[1]);
	}
MikePath.prototype.slopeNormal_byTheta = function()  {
	var seg=this.getSegThetaIndexAtPathTheta(arguments[0]);
	return seg[0].slopeNormal_byTheta(seg[1]);
	}
MikePath.prototype.angleTangent = function() {
	var res = new Array();
	var sir = this.getSegsInRange(new Array(arguments[0],arguments[1]));
	for (var i=0;i<sir.length;i++) {
		res.push(sir[i].angleTangent(arguments[0],arguments[1]));
		}
	return (arguments[2])?res:res[0];//hmm, this is instead of PERL's wantarray 
	}
MikePath.prototype.angleNormal = function()  {
	var res = new Array();
	var sir = this.getSegsInRange(new Array(arguments[0],arguments[1]));
	for (var i=0;i<sir.length;i++) {
		var as=sir[i].angleNormal(arguments[0],arguments[1]);
		for (var j=0;j<as.length;j++) {res.push(as[j]);}
		}
	return (arguments[2])?res:res[0];//hmm, this is instead of PERL's wantarray 
	}
MikePath.prototype.angleTangent_byTheta = function() {
	var seg=this.getSegThetaIndexAtPathTheta(arguments[0]);
	return seg[0].angleTangent_byTheta(seg[1]);
	}
MikePath.prototype.angleNormal_byTheta = function()  {
	var seg=this.getSegThetaIndexAtPathTheta(arguments[0]);
	return seg[0].angleNormal_byTheta(seg[1]);
	}

MikePath.prototype.normalizeY = function() {
	var nymin = arguments[0];
	var nymax = arguments[1];
	var constrain = (arguments[2])?arguments[2]:false;
	var scalefactor=1;
	var ydiff=this.maxy - this.miny;
	var oldx=this.minx;
	//this.translate(-this.minx,-this.miny);
	if (Math.abs(ydiff) > 0.0000000000001) {scalefactor = (nymax - nymin)/ydiff;}
	var newspec='';
	//alert("yoldspec:"+this.pathspec+"\n");
	for (var i=0;i<this.pathSegmentSpecs.length;i++) {
		var segTypeLetter = this.pathSegmentSpecs[i].substring(0,1);
		var thesepoints= extractPointsFromPathSpec(this.pathSegmentSpecs[i]);
		//if (!constrain) {newspec+=segTypeLetter + thesepoints.map(function(x) {return typeof(x)=='object'? x[0] +              ','+(x[1] * scalefactor) : x}).join(',');}
		//else            {newspec+=segTypeLetter + thesepoints.map(function(x) {return typeof(x)=='object'?(x[0] * scalefactor)+','+(x[1] * scalefactor) : x}).join(',');}
		newspec+=segTypeLetter;
		var first=true;
		for (var j=0;j<thesepoints.length;j++) {
			//alert(typeof(thesepoints[j]));
			if (typeof(thesepoints[j]) == 'object') {
				var newy=thesepoints[j][1] * scalefactor;
				var newx=thesepoints[j][0];
				//if (constrain && !(first && segTypeLetter == 'A')) {
				if (constrain || ( segTypeLetter == 'A')) {
					newx*=scalefactor;
					}
				newspec+=newx+','+newy+',';
				}
			else {newspec+=thesepoints[j]+',';}
			first=false;
			}
		newspec=newspec.substring(0,newspec.length - 1);
		}

	//alert("ynewspec: "+newspec + "\n");
	this.constructSegments(newspec);
	var offset = nymin - this.miny;
	this.translate(oldx,offset);
	}
MikePath.prototype.normalizeX = function() {
	var nxmin = arguments[0];
	var nxmax = arguments[1];
	var constrain = (arguments[2])?arguments[2]:0;
	var scalefactor=1;
	var untranslatex=parseFloat(this.pathSegments[0].p1[0]);
	var untranslatey=parseFloat(this.pathSegments[0].p1[1]);
	//this.translate(-this.pathSegments[0].p1[0],-this.pathSegments[0].p1[1]);
	var xdiff=this.maxx - this.minx;
	//alert(this.pathspec + "   minmax: " +this.maxx +" - "+ this.minx);
	if (Math.abs(xdiff) > 0.0000000000001) {scalefactor = (nxmax - nxmin)/xdiff;}

	var newspec='';
	//alert("xoldspec:"+this.pathspec+" has how many segspecs:"+this.pathSegmentSpecs.length+"\n");
	for (var i=0;i<this.pathSegmentSpecs.length;i++) {
		var segTypeLetter = this.pathSegmentSpecs[i].substring(0,1);
        if (segTypeLetter == 'Z' || segTypeLetter == 'z') {continue;}
		var thesepoints= extractPointsFromPathSpec(this.pathSegmentSpecs[i]);
		//if (!constrain) {newspec+=segTypeLetter + thesepoints.map(function(x) {return typeof(x)=='object'?(x[0] * scalefactor)+','+x[1] : x;               }).join(',');}
		//else            {newspec+=segTypeLetter + thesepoints.map(function(x) {return typeof(x)=='object'?(x[0] * scalefactor)+','+(x[1] * scalefactor) : x}).join(',');}
		newspec+=segTypeLetter;
		var first=true;
		//alert(segTypeLetter);
		for (var j=0;j<thesepoints.length;j++) {
			//alert(typeof(thesepoints[j]));
			if (typeof(thesepoints[j]) == 'object') {

				var newx=thesepoints[j][0] * scalefactor;
				var newy=thesepoints[j][1];
				if (constrain || ( segTypeLetter == 'A')) {//HMMM need to really think through this situation - should be simple, but I keep blindly fooling with it.
					newy*=scalefactor;
					}
				newspec+=newx+','+newy+',';
				}
			else {newspec+=thesepoints[j]+',';}
			first=false;
			}
		newspec=newspec.substring(0,newspec.length - 1);//chop off last comma

		}
	//alert("xnewspec: " + newspec + "\n");
	this.constructSegments(newspec);
	var offset = nxmin - this.minx;
	//this.translate(offset,untranslatey);
	}


//foreach (@{$self->{pathSegmentSpecs}}) {
//	if (substr($_,0,1) eq 'A') {
//		my @pts=&extractPointsFromPathSpec($_);
//		$newspec.=substr($_,0,1) . join(',',map {(ref($_) eq 'ARRAY')?($_->[0]).','.($_->[1]) : $_} @pts[0 .. $#pts - 1]);
//		$newspec.=','.eval(sprintf("%.9f",$pts[$#pts]->[0] + $xoff)).','.eval(sprintf("%.9f",$pts[$#pts]->[1] + $yoff));
//		}
//	else {
//		#$newspec.=substr($_,0,1) . join(',',map {eval(sprintf("%.9f",$_->[0] + $xoff)).','.eval(sprintf("%.9f",$_->[1] + $yoff))} &extractPointsFromPathSpec($_));
//		#$newspec.=substr($_,0,1) . join(',',map {eval(substr($_->[0] + $xoff,0,25)).','.eval(substr($_->[1] + $yoff,0,25))} &extractPointsFromPathSpec($_));
//		$newspec.=substr($_,0,1) . join(',',map {($_->[0] + ((ref($_->[0])&&ref($xoff))?$xoffbig:$xoff)).','.($_->[1] + ((ref($_->[1])&&ref($yoff))?$yoffbig:$yoff))} &extractPointsFromPathSpec($_));
//		}
//	}
//$self->constructSegments($newspec);
	
MikePath.prototype.translate = function() {
	var xoff = arguments[0];
	var yoff = arguments[1];
	if (!xoff) {xoff=0};
	if (!yoff) {yoff=0};
	//alert("translate:oldspec:\n"+this.pathspec+"\n");
	//var osp = "scale:oldspec:"+this.pathspec +"\n";
	var newspec='';
	for (var i=0;i<this.pathSegmentSpecs.length;i++) {
		var pts=extractPointsFromPathSpec(this.pathSegmentSpecs[i]);
		if (this.pathSegmentSpecs[i].substring(0,1) == 'A') {
			newspec+=this.pathSegmentSpecs[i].substring(0,1);
			for (var j=0;j<pts.length - 1;j++) {
				newspec+=((typeof(pts[j]) == 'object')?(pts[j][0])+','+(pts[j][1]) : pts[j]) + ',';
				}
			newspec=newspec.substring(0,newspec.length - 1);
			newspec+=','+(parseFloat(pts[pts.length - 1][0]) + xoff)+','+(parseFloat(pts[pts.length - 1][1]) + yoff);
			}
		else {
            // pts > 1 is to avoid Z (when Z doesn't have a hacked-in filled in point following)
			newspec+=this.pathSegmentSpecs[i].substring(0,1) + (pts[pts.length-1] != "" ? pts.map(function(x) {return (parseFloat(x[0]) + xoff)+','+(parseFloat(x[1]) + yoff);}).join(',') : "" );}
			}
	this.constructSegments(newspec);
	//alert("translate: newspec:\n"+newspec+"\ncheck\n"+this.pathspec+"\n");
	//alert(osp+"scale:newspec:\n"+newspec+"\n");
	}

// reletively new, as far as use
// found bug in 'A' arc handling that was maybe never triggered
// could be others
// think I mainly use this to scale planshape for rail placement display
// made Perl copy of this, but haven't used it in Perl context
// so this is the main version, and bug fixes here should transfer over there
MikePath.prototype.scale = function() {
	var xscale = arguments[0];
	var yscale = arguments[1];
	// these two ifs were just "fixed" from being inertly wrong
	// to being maybe right, but untested. synced with Perl version.
	// but I only use this scale function in a JS context at the moment.
	if (!xscale && yscale) {xscale=1;}
	if (!yscale && xscale) {yscale=xscale;}
	var osp = "scale:oldspec:"+this.pathspec +"\n";
	var newspec='';
	for (var i=0;i<this.pathSegmentSpecs.length;i++) {
		var pts=extractPointsFromPathSpec(this.pathSegmentSpecs[i]);
		if (this.pathSegmentSpecs[i].substring(0,1) == 'A') {
			newspec+=this.pathSegmentSpecs[i].substring(0,1);
			for (var j=0;j<pts.length - 1;j++) {newspec+=(typeof(pts[j]) == 'Array')?(pts[j][0])+','+(pts[j][1]) : pts[j]+','};
			newspec=newspec.substring(0,newspec.length - 1);
			newspec+=','+(parseFloat(pts[pts.length - 1][0]) * xscale)+','+(parseFloat(pts[pts.length - 1][1]) * yscale);
			}
		else {
            // pts > 1 is to avoid Z (when Z doesn't have a hacked-in filled in point following)
			newspec+=this.pathSegmentSpecs[i].substring(0,1) + (pts[pts.length-1] != "" ? pts.map(function(x) {return (parseFloat(x[0]) * xscale)+','+(parseFloat(x[1]) * yscale);}).join(',') : "");}
			}
	this.constructSegments(newspec);
	//alert(osp+"scale:newspec:\n"+newspec+"\n");
	}
MikePath.prototype.constructSegment = function() {
	var segspec = arguments[0];
	var segspecprevious = arguments[1];
	var segTypeLetter = segspec.substring(0,1);
	var lastpoints = extractPointsFromPathSpec(segspecprevious);
	var thesepoints= extractPointsFromPathSpec(segspec);
	if      (segTypeLetter == 'C') {return new BezierCubicSegment(lastpoints[lastpoints.length-1],thesepoints[0],thesepoints[1],thesepoints[2],this.precision);}
	else if (segTypeLetter == 'L') {return new LineSegment(lastpoints[lastpoints.length-1],thesepoints[0],this.precision);}
	else if (segTypeLetter == 'H') {
        thesepoints = [thesepoints[0],lastpoints[lastpoints.length-1][1]];
	    return new LineSegment(lastpoints[lastpoints.length-1],thesepoints[0],this.precision);
        }
	else if (segTypeLetter == 'V') {
        thesepoints = [lastpoints[lastpoints.length-1][0],thesepoints[0]];
	    return new LineSegment(lastpoints[lastpoints.length-1],thesepoints[0],this.precision);
        }
	else if (segTypeLetter == 'M') {return new MoveTo(     lastpoints[lastpoints.length-1],thesepoints[0],this.precision);}
	else if (segTypeLetter == 'Z') {return new ClosePath(  lastpoints[lastpoints.length-1],thesepoints[0],this.precision);}
	else if (segTypeLetter == 'z') {return new ClosePath(  lastpoints[lastpoints.length-1],thesepoints[0],this.precision);}
	else if (segTypeLetter == 'A') {return new EllipticalArc(lastpoints[lastpoints.length-1],thesepoints[0],thesepoints[1],thesepoints[2],thesepoints[3],thesepoints[4],this.precision);}
	}
function extractPointsFromPathSpec() {
	var segspec = new String(arguments[0]);
	var segTypeLetter = segspec.substring(0,1);
	if (segTypeLetter == 'M') {     return new Array(segspec.substring(1,segspec.length).split(/[ ,]+/));}
	else if (segTypeLetter == 'L') {return new Array(segspec.substring(1,segspec.length).split(/[ ,]+/));}
	else if (segTypeLetter == 'H') {return new Array(segspec.substring(1,segspec.length));}
	else if (segTypeLetter == 'V') {return new Array(segspec.substring(1,segspec.length));}
	else if (segTypeLetter == 'Z') {return new Array(segspec.substring(1,segspec.length).split(/[ ,]+/));}
	else if (segTypeLetter == 'z') {return new Array(segspec.substring(1,segspec.length).split(/[ ,]+/));}
	else if (segTypeLetter == 'C') {var pts = segspec.substring(1,segspec.length).split(/[ ,]+/);return new Array(new Array(pts[0],pts[1]),new Array(pts[2],pts[3]),new Array(pts[4],pts[5]))}
	else if (segTypeLetter == 'A') {var pts = segspec.substring(1,segspec.length).split(/[ ,]+/);return new Array(new Array(pts[0],pts[1]),pts[2],pts[3],pts[4],new Array(pts[5],pts[6]))}
	}
MikePath.prototype.getFeet = function() {
	var feet=new Array();
	for (var i=0;i<this.pathSegments.length;i++) {
		var f=this.pathSegments[i].getFeet(arguments[0],arguments[1]);
		if (f.length>0) {
			for (var j=0;j<f.length;j++) {f[j][2]=this.getPathThetaAtSegTheta(i,f[j][2]);}
			feet=feet.concat(f);
			}
		}
	return feet;
	}
MikePath.prototype.getIntersections = function() {
	var other= arguments[0];
	var wantThetas= arguments.length>1 ?arguments[1]:false;	
	var intersects=new Array();
	for (var i=0;i<this.pathSegments.length;i++) {
//var thstr='thisseg:['+this.pathSegments[i].p1 + '] , [' + this.pathSegments[i].p2+']';
		for (var j=0;j<other.pathSegments.length;j++) {
			var theseintersects=getSegSegIntersects(this.pathSegments[i],other.pathSegments[j],wantThetas);
// some debug stuff? did I finish that debug? or is bug still in there?
//if (theseintersects.length>0) {
//	thstr+='\n['+i+','+j+']:'+this.pathSegments[i]+':'+other.pathSegments[j]+']';
//	thstr+='\n other seg p1,p2:['+other.pathSegments[j].p1 + '] , [' + other.pathSegments[j].p2+']';
//	}
			for (var k=0;k<theseintersects.length;k++) {
				if (wantThetas) {
					theseintersects[k]=this.getPathThetaAtSegTheta(i,theseintersects[k]);
					}
				intersects[intersects.length]=theseintersects[k];
				}
			}
		}
//if (intersects.length==3) {throw thstr+'\n3INTERSECTS:\n'+intersects;}
	return intersects;
	}
function getIntersections() {
	var one  = arguments[0];
	var other= arguments[1];
	var wantThetas= arguments.length>2 ?arguments[2]:false;
	var intersects=new Array();
	for (var i=0;i<one.pathSegments.length;i++) {
		for (var j=0;j<other.pathSegments.length;j++) {
			var theseintersects=getSegSegIntersects(one.pathSegments[i],other.pathSegments[j],wantThetas);
			for (var k=0;k<theseintersects.length;k++) {
				if (wantThetas) {
					theseintersects[k]=this.getPathThetaAtSegTheta(i,theseintersects[k]);
					}
				intersects[intersects.length]=theseintersects[k];
				}
			}
		}
	return intersects;
	}
function getSegSegIntersects() {
	var seg1=arguments[0];
	var seg2=arguments[1];
	var wantThetas = arguments.length>2 ? arguments[2]:false;
	var wantNativeThetas = arguments.length>3 ? arguments[3]:0;
	var refstrings = seg1.segType + '--' + seg2.segType;
	var ret=new Array();
	//if (refstrings.indexOf('LineSegment')!=-1 && refstrings.indexOf('BezierCubicSegment')!=-1) {
	if (refstrings == 'LineSegment--BezierCubicSegment' || refstrings == 'BezierCubicSegment--LineSegment' ||
		refstrings == 'ClosePath--BezierCubicSegment' || refstrings == 'BezierCubicSegment--ClosePath') {
		var lineIsSelf = (refstrings == 'LineSegment--BezierCubicSegment' || refstrings == 'ClosePath--BezierCubicSegment')?true:false;
		var line  = (lineIsSelf)?seg1:seg2;
		var curve = (lineIsSelf)?seg2:seg1;
		//# t^3 + [(F-mB)/(E-mA)]t^2 + [(G-mC)/(E-mA)]t + [(H-md-x0)/(E-mA)]

        // TODO: perl version handles some special cases first - port those over here

		var cubicresult = cubicformula(
		  (curve._F-(line.m*curve.B))        / (curve.E-(line.m*curve.A)),
		  (curve.G-(line.m*curve.C))        / (curve.E-(line.m*curve.A)),
		  (curve.H-(line.m*curve.D)-line.b) / (curve.E-(line.m*curve.A)),
		  1);
		var thetas=new Array();
		for (var i=0;i<cubicresult.length;i++) {
			//if (this.precision + 1 > cubicresult[i] && cubicresult[i] > 0 - this.precision) {thetas[thetas.length]=cubicresult[i];}
			if (1 > cubicresult[i] && cubicresult[i] > 0) {thetas[thetas.length]=cubicresult[i];}
			}
		thetas.sort(numsrt);
		for (var i=0;i<thetas.length;i++) {
			var x = curve.bezierEvalXofT(thetas[i]);
			//if (x <= line.maxx + line.precision && x >= line.minx - line.precision) {
			if (x <= line.maxx && x >= line.minx) {
				if (wantThetas) {
					if (!lineIsSelf) {ret.push(thetas[i]);}
					else {ret.push(line.solveXforTheta(x));}
					}
				else {
					ret[ret.length] = new Array( curve.bezierEvalXofT(thetas[i]) , curve.bezierEvalYofT(thetas[i]) );
					}
				}
			}
		}
	else if (refstrings == 'LineSegment--LineSegment' || 
			refstrings == 'LineSegment--ClosePath' ||
			refstrings == 'ClosePath--ClosePath' ||
			refstrings == 'ClosePath--LineSegment'
			) {

		
		//# m1x + b1 = m2x + b2
		//# (m1-m2)x = b2-b1
		//# x = (b2-b1)/(m1-m2)
		
		
		
		/* 
		var x;
		var dm = seg1.m - seg2.m;
		if      (!isFinite(seg1.m) && isFinite(seg2.m)) {x = seg1.p1[0];}
		else if (!isFinite(seg2.m) && isFinite(seg1.m)) {x = seg2.p1[0];}
		else if (Math.abs(dm) > 0) {x=(seg2.b - seg1.b)/dm;}
		if (isFinite(seg1.m)) {
			if (!isFinite(seg2.m) &&   (seg2.p2[0]<seg1.minx || seg2.p2[0]>seg1.maxx) ) {
				//nothing
				}
			else if (!isNaN(x) &&
				x <= seg1.maxx && 
				x >= seg1.minx && 
				x <= seg2.maxx && 
				x >= seg2.minx) {
				var y=(seg1.m*x)+seg1.b;
				if ( !isFinite(seg2.m) &&    // in this case, the x set above isn't a calculated seg-line intersection, it's just the constant x of the other segment. If this segment's calculated y isn't in the other's y range, no intersection.
					(y<seg2.miny || y>seg2.maxy)
					) { //alert('nothing ' +"\n"+'x : ' + x +"\n"+'m1: '+seg1.m+"\n"+'m2: '+seg2.m+"\n"+'b1: '+seg1.b+"\n"+'b2: '+seg2.b+"\n");
					//nothing
					}
				//if (!(y<seg2.miny || y>seg2.maxy)) {
				else {
					if (wantThetas) {ret.push(seg1.solveXforTheta(x));}
					else {ret[ret.length]= new Array(x,y);}
					}
				}
			}
		else if (isFinite(seg2.m)) {
			var y = (seg2.m*x)+seg2.b;
			if (!isNaN(y) &&
				y <= seg1.maxy && 
				y >= seg1.miny && 
				y <= seg2.maxy && 
				y >= seg2.miny) {
				if (wantThetas) {ret.push(seg1.solveYforTheta(y));}
				else {ret[ret.length]= new Array(x,y);}
				}
			}
		*/
		/* REPLACE THAT WITH TRANSLATION OF BEST-SO-FAR APPROACH FROM path_to_offset_segments.pl: */







		/*  from perl version function: sub seg_seg_intersection { */
		//my $seg1=shift;
		//my $seg2=shift;

		var segsegret=false;

		var x1= seg1.p1[0];var y1= seg1.p1[1];
		var x2= seg1.p2[0];var y2= seg1.p2[1];
		var u1= seg2.p1[0];var v1= seg2.p1[1];
		var u2= seg2.p2[0];var v2= seg2.p2[1];

		//var m1 = (x2 == x1)?'Inf':(y2 - y1)/(x2 - x1);
		//var m2 = (u2 == u1)?'Inf':(v2 - v1)/(u2 - u1);
		var m1 = seg1.m;
		var m2 = seg2.m;

		var b1;
		var b2;

		var xi;
		var dm = m1 - m2; // one or both might be Infinity...then what?
		//if    (m1 == 'Inf' && !(m2 == 'Inf')) {xi = x1;b2 = v1 - (m2 * u1);}
		if    (!isFinite(m1) && isFinite(m2)) {xi = x1;b2 = v1 - (m2 * u1);}
		//else if (m2 == 'Inf' && !(m1 == 'Inf')) {xi = u1;b1 = y1 - (m1 * x1);}
		else if (!isFinite(m2) && isFinite(m1)) {xi = u1;b1 = y1 - (m1 * x1);}
		else if (Math.abs(dm) > 0.000000000001) {
			b1 = y1 - (m1 * x1);
			b2 = v1 - (m2 * u1);	
			xi=(b2-b1)/dm;
			}
		var lowhiu = (u2>u1) ? new Array(u1,u2):new Array(u2,u1);
		if (isFinite(m1)) {
			var lowhix = (x2>x1) ? new Array(x1,x2):new Array(x2,x1);
			if (!isFinite(m2) &&   ( u2 < lowhix[0] || u2 > lowhix[1] ) ) {
				//return [];
				//not returning in this context
				}
			//#if (!isNaN(xi) &&  <-- this came from best perl version, commented out there, but obviously it's a remnant of some javascript version that preceded that. long evolution.
			else if (
				(xi || xi == 0) &&
				(xi < lowhix[1] || xi == lowhix[1]) && 
				(xi > lowhix[0] || xi == lowhix[0]) &&
				(xi < lowhiu[1] || xi == lowhiu[1]) && 
				(xi > lowhiu[0] || xi == lowhiu[0])
				) {
				var y=(m1*xi)+b1;
				var lowhiv = (v2>v1) ? new Array(v1,v2):new Array(v2,v1);



				if (
					!isFinite(m2) &&
					(y < lowhiv[0] || y > lowhiv[1])
					) {
					//return [];
					//not returning in this context
					}
				else if ( // was trying to fix two competeing problems cases, one with one m==0, the other with one m==Inf
				          // the sort of blind-thought-through attempts weren't working, but this prod/guess did work for both cases
					v1 != v2 &&  // though this should be y1 != y2, but that didn't do it. Maybe the segs are being processed in opposite order than I imagine. anyway, worked when I tried this v1 != v2
					             // ... in other words, this else if () feels like a hack, even if by accident it's sound. 
								 // would like to work out the ultimate seg_seg intersector algorithm someday - it would look different from all this.
					(y < lowhiv[0] || y > lowhiv[1])
					
					) {
					// nothing
					}
				else {
					if (wantThetas) {
						segsegret = seg1.solveXforTheta(xi);
						}
					else {
						segsegret = new Array(xi,y);
						}
					}






				}
			}
		else if (isFinite(m2)) { // #so $m1 is Inf
			if (x1 < lowhiu[0] || x1 > lowhiu[1] && ! (x1 == lowhiu[0] || x1 == lowhiu[1])) {
				//return [];
				//not returning in this context
				}
			else {
				var lowhiy = (y2>y1) ? new Array(y1,y2):new Array(y2,y1);
				var lowhiv = (v2>v1) ? new Array(v1,v2):new Array(v2,v1);
				var yi = (m2*xi)+b2;
				//#print "$x1,$y1,$x2,$y2\n  $yi = ($m2*$xi)+$b2;\n";
				if ((yi || yi == 0) &&
					(yi < lowhiy[1] || yi == lowhiy[1]) && 
					(yi > lowhiy[0] || yi == lowhiy[0]) &&
					(yi < lowhiv[1] || yi == lowhiv[1]) && 
					(yi > lowhiv[0] || yi == lowhiv[0])
					) {
					if (wantThetas) {segsegret = seg1.solveYforTheta(yi);}
					else {segsegret=new Array(xi,yi);}
					}
				}
			}
//		alert(segsegret);
		//if ((wantThetas && segsegret) || segsegret.length == 2) {
		if (segsegret) {
			ret[ret.length] = segsegret;
			}
















		}
	return ret;
	}


function BezierCubicSegment(p1,cp1,cp2,p2,precision) {
	this.segType='BezierCubicSegment';
	this.p1 =new Array(new Number(p1[0]) ,new Number(p1[1]));
	this.cp1=new Array(new Number(cp1[0]),new Number(cp1[1]));
	this.cp2=new Array(new Number(cp2[0]),new Number(cp2[1]));
	this.p2 =new Array(new Number(p2[0]) ,new Number(p2[1]));
	this.precision = precision;
	var coordsarray = new Array(new Array(this.p1[0],this.p1[1],this.cp2[0],this.cp2[1]),new Array(this.cp1[0],this.cp1[1],this.p2[0],this.p2[1]));
	var diags = new Array;
	for (var i=0;i<coordsarray.length;i++) {
		diags[diags.length] = Math.sqrt( Math.pow(coordsarray[i][3] - coordsarray[i][2],2) + Math.pow(coordsarray[i][1] - coordsarray[i][0],2));
		}
	this.maxdiaglength	= Math.min(diags);
	this.thetaprecision = this.precision/this.maxdiaglength;
	this.A  = this.p2[0] - 3 * this.cp2[0] + 3 * this.cp1[0] -     this.p1[0];
	this.B  =              3 * this.cp2[0] - 6 * this.cp1[0] + 3 * this.p1[0];
	this.C  =                                3 * this.cp1[0] - 3 * this.p1[0];
	this.D  =                                                      this.p1[0];
	this.E  = this.p2[1] - 3 * this.cp2[1] + 3 * this.cp1[1] -     this.p1[1];
	// conflicts with function named F(), so this F is now _F (toto I don't think we're in Perl anymore)
	this._F =              3 * this.cp2[1] - 6 * this.cp1[1] + 3 * this.p1[1];
	this.G  =                                3 * this.cp1[1] - 3 * this.p1[1];
	this.H  =                                                      this.p1[1];
	this.BdA=this.B/this.A;
	this.CdA=this.C/this.A;
	this.FdE=this._F/this.E;
	this.GdE=this.G/this.E;
	this.Am3=this.A*3;
	this.Bm2=this.B*2;
	this.Em3=this.E*3;
	this.Fm2=this._F*2;
	this.Am6=this.A*6;
	this.Em6=this.E*6;

	this.extremexsthetas = this.bezierSolveXPrimeforTheta(0);
	this.extremexsthetas.unshift(0);
	this.extremexsthetas.push(1);
	this.extremexs=new Array();
	for (var i=0;i<this.extremexsthetas.length;i++) {this.extremexs[this.extremexs.length]=this.bezierEvalXofT(this.extremexsthetas[i]);}
	this.extremexs[this.extremexs.length]=this.p1[0];
	this.extremexs[this.extremexs.length]=this.p2[0];
	this.extremexs=this.extremexs.sort(numsrt);
	this.minx = this.extremexs[0];
	this.maxx = this.extremexs[this.extremexs.length - 1];

	this.extremeysthetas = this.bezierSolveYPrimeforTheta(0);
	this.extremeysthetas.unshift(0);
	this.extremeysthetas.push(1);
	this.extremeys=new Array();
	for (var i=0;i<this.extremeysthetas.length;i++) {this.extremeys[this.extremeys.length]=this.bezierEvalYofT(this.extremeysthetas[i]);}
	//this.extremeys[this.extremeys.length]=this.p1[1];
	//this.extremeys[this.extremeys.length]=this.p2[1];
	this.extremeys=this.extremeys.sort(numsrt);
	this.miny = this.extremeys[0];
	this.maxy = this.extremeys[this.extremeys.length - 1];
	this.length;
	}
//BezierCubicSegment.prototype.getLength = function() {
//	var stepsize=arguments.length>0?arguments[0]:Math.sqrt(Math.pow(this.maxx - this.minx,2) + Math.pow(this.maxy - this.miny,2))/1000;
//	if (arguments.length>0 || !this.length) {
//		var pts=new Array();
//		var prevtheta=0;
//		var firstpt=this.point(prevtheta);
//		pts.push(firstpt);
//		var thispointandtheta=new Array(pts,this.angleNormal(pts[0]));
//		endpt=this.point(1);
//		while (Math.sqrt(Math.pow(endpt[1]-thispointandtheta[0][1],2) + Math.pow(endpt[0]-thispointandtheta[0][0],2)) > stepsize) {
//			thispointandtheta=this.dimensionalStepFromTheta(stepsize,prevtheta);
//			pts.push(thispointandtheta[0]);
//			prevtheta=thispointandtheta[1];
//			}
//		pts.push(endpt);
//		this.length=0;
//		for (var i=1;i<pts.length;i++) {this.length+=Math.sqrt(Math.pow(pts[i][0] - pts[i-1][0],2) + Math.pow(pts[i][1] - pts[i-1][1],2));}
//		}
//	return this.length;
//	}
BezierCubicSegment.prototype.getLength = function() {
	var res = arguments.length ? arguments[0] : 1000;
	var start_theta = arguments.length > 1 ? arguments[1] : 0;
	var end_theta   = arguments.length > 2 ? arguments[2] : 1;
	if (end_theta<start_theta) {var tmp=start_theta;start_theta=end_theta;end_theta=tmp;}
	if (end_theta == start_theta) {return 0;}
	var length=0;
	var inc = (end_theta - start_theta)/res;
	for (var t=start_theta + inc;t<=end_theta;t+=inc) {
		length+=Math.sqrt(Math.pow(this.bezierEvalXofT(t) - this.bezierEvalXofT(t - inc),2) + Math.pow(this.bezierEvalYofT(t) - this.bezierEvalYofT(t - inc),2));
		}
	return length;
	}
BezierCubicSegment.prototype.precision = function() {
	if (typeof parseFloat(arguments[0]) == "number" && !isNaN(parseFloat(arguments[0]))) {this.precision=parseFloat(arguments[0]);this.thetaprecision = this.precision/this.maxcoordval;}
	return this.precision;
	}
BezierCubicSegment.prototype.getRange = function() {
	return (this.minx,this.miny,this.maxx,this.maxy);
	}
BezierCubicSegment.prototype.inRange = function() {
	var coords = arguments[0];
	var xok=false;
	var yok=false;
	if (typeof parseFloat(coords[0]) == "number" && !isNaN(parseFloat(coords[0])) && this.minx - this.precision <= coords[0] && this.precision + this.maxx >= coords[0]) {xok=true;}
	if (typeof parseFloat(coords[1]) == "number" && !isNaN(parseFloat(coords[1])) && this.miny - this.precision <= coords[1] && this.precision + this.maxy >= coords[1]) {yok=true;}
	return new Array(xok,yok);
	}
BezierCubicSegment.prototype.onSegment = function() {
	var point = arguments[0];
	var ar = this.solveXforTheta(point[0]);
	var retcnt = 0;
	for (var i=0;i<ar.length;i++) {if (point[1] - this.bezierEvalYofT(ar[i]) < this.precision) {retcnt++;}}
	return retcnt;
	}
BezierCubicSegment.prototype.getFeet = function() {
	var x=parseFloat(arguments[0]);
	var y=parseFloat(arguments[1]);
	//#for each interval between critical points - critical due to features of x(i) and y(i). - hueristic and then root find to get any 90 degree intersections
	var feet=new Array();
	var ds=new Object(); // need associative array behavior here
	var dangers=new Array();
	for (var i=0;i<this.extremeysthetas.length;i++) {if (!ds[this.extremeysthetas[i]]) {ds[this.extremeysthetas[i]]=true;dangers.push(this.extremeysthetas[i])}}
	for (var i=0;i<this.extremexsthetas.length;i++) {if (!ds[this.extremexsthetas[i]]) {ds[this.extremexsthetas[i]]=true;dangers.push(this.extremexsthetas[i])}}
	dangers.sort(function(a,b) {return (a - b);}); 
	for (var i=1;i<dangers.length;i++) {
		var boundl=dangers[i - 1];
		var boundr=dangers[i];
		var find90 = function () { //#dotproduct equals zero for two perpendicular vectors
			return (this.bezierEvalYofT(arguments[0]) - y) * this.slopeTangent_byTheta(arguments[0]) + (this.bezierEvalXofT(arguments[0]) - x); //# * $tanvec->[1] ( which is == 1 )
			};
		var find90_other = function() { //#dotproduct equals zero for two perpendicular vectors
			return (this.bezierEvalXofT(arguments[0]) - x) * this.slopeNormal_byTheta(arguments[0]) + -1 * (this.bezierEvalYofT(arguments[0]) - y); //# * $tanvec->[1] ( which is == 1 )
			};
		var st1=this.slopeTangent_byTheta(boundl);
		var st2=this.slopeTangent_byTheta(boundr);
		var functouse =find90;
		if (Math.abs(st1) > 1000 || Math.abs(st2) > 1000 || !isFinite(st1) || !isFinite(st2)) {
			functouse = find90_other;
			}
		var b_ret=BrentsMethod(this,functouse,new Array(boundl,boundr),this.precision/1000,undefined,'trying to find feet on Bezier in MikePath');
		if (!b_ret[1]) {
			feet.push(new Array(this.bezierEvalXofT(b_ret[0]),this.bezierEvalYofT(b_ret[0]),b_ret[0]));
			}
		else {
			if (b_ret[1].indexOf('too many') != -1) { //#root finding hit some iteration limit
				throw "bailed in getFeet for bezier, root find reported too many iterations\n" + "too many\nx:"+x+",y:"+y+"\nbezspec:\n ["+this.p1[0]+","+this.p1[1]+"]\n ["+this.cp1[0]+","+this.cp1[1]+"]\n ["+this.cp2[0]+","+this.cp2[1]+"]\n ["+this.p2[0]+","+this.p2[1]+"]\n";
				}
			else {
				// Would you consider adding "false feet" to what this returns? should happen at path level, but segs could figure what they would be. maybe do that with flag saying you want them.
				//throw "bailed in getFeet for bezier: " + b_ret[1] +"]\n";
				}
			}
		}
	return feet;
	}

BezierCubicSegment.prototype.nuth = function() {return 1;} // what the hell is this?

BezierCubicSegment.prototype.slopeTangent = function() {
	var x = parseFloat(arguments[0]);
	var y = (typeof parseFloat(arguments[1]) == "number" && !isNaN(parseFloat(arguments[1])))?parseFloat(arguments[1]):false;
	var thetas=new Array();
	var ret=new Array();
	if    (typeof(x) == "number" && !isNaN(x)) {thetas = this.solveXforTheta(x);}
	else if (typeof(y) == "number") {thetas = this.solveYforTheta(y);}
	else {return ret;}
	for (var i=0;i<thetas.length;i++) {
		var yp=this.bezierEvalYPrimeofT(thetas[i]);
		var xp=this.bezierEvalXPrimeofT(thetas[i]);
		if (xp == '0') {ret[ret.length]=(((yp < 0)?-1:1)*1)/0;} // +/- Infinity
		if (yp == '0') {ret[ret.length]=parseInt(((xp < 0)?'-':'+')+'0');} // sign probably won't stick
		else {ret[ret.length]=yp/xp;}
		}
	return ret;
	}
BezierCubicSegment.prototype.slopeTangent_byTheta = function() {
	var theta = parseFloat(arguments[0]);
	var yp=this.bezierEvalYPrimeofT(theta);
	var xp=this.bezierEvalXPrimeofT(theta);
	if (xp == 0) {return ((yp < 0)?-1:1) / 0;} //  +/- Infinity
	if (yp == 0) {return parseInt(((xp < 0)?'-':'+')+'0');} // sign probably won't stick
	else {return yp/xp;}
	}
BezierCubicSegment.prototype.angleTangent = function() {
	var x = parseFloat(arguments[0]);
	var y = (typeof parseFloat(arguments[1]) == "number" && !isNaN(parseFloat(arguments[1])))?parseFloat(arguments[1]):false;
	var thetas=new Array();
	var ret=new Array();
	if    (typeof(x) == "number" && !isNaN(x)) {thetas = this.solveXforTheta(x);}
	else if (typeof(y) == "number") {thetas = this.solveYforTheta(y);}
	else {return ret;}
	for (var i=0;i<thetas.length;i++) {
		var yp=this.bezierEvalYPrimeofT(thetas[i]);
		var xp=this.bezierEvalXPrimeofT(thetas[i]);
		//if (xp == '0') {ret[ret.length]=(((yp < 0)?-1:1)*1)/0;} // +/- Infinity
		//if (yp == '0') {ret[ret.length]=parseInt(((xp < 0)?'-':'+')+'0');} // sign probably won't stick
		//else {
			ret[ret.length]=Math.atan2(yp,xp);
		//	}
		}
	return ret;
	}
BezierCubicSegment.prototype.slopeNormal = function() {
	var x = parseFloat(arguments[0]);
	var y = (typeof(parseFloat(arguments[1])) == "number" && !isNaN(parseFloat(arguments[1])))?parseFloat(arguments[1]):false;
	var ret = new Array();
	var slopetans=this.slopeTangent(x,y);
	for (var i=0;i<slopetans.length;i++) {
		var negRecip;
		var thisslopetan = new String(slopetans[i]);
		if (thisslopetan.match(/([\-\+]?)inf$/i)) { // javascript will deliver Infinity or -Infinity
			var sign = '';
			var regmatch=new String(RegExp.$1);
			if (regmatch.length) {if (regmatch == '-') {sign='+';} else {sign='-';}}
			negRecip=parseInt(sign+'0');
			}
		else if (thisslopetan.match(/^([\-\+]?)0$/)) {
			var sign = '';
			var regmatch=new String(RegExp.$1);
			if (regmatch.length) {if (regmatch == '-') {sign='+';} else {sign='-';}}
			negRecip=(parseInt(sign+'1') * 1)/0;
			}
		else {
			negRecip = -1/slopetans[i];
			}
		ret[ret.length]=negRecip;
		}
	return ret;
	}
BezierCubicSegment.prototype.slopeNormal_byTheta = function() {
	var theta = parseFloat(arguments[0]);
	var slopeTangent = this.slopeTangent_byTheta(theta);
	return -1/slopeTangent;
	}
BezierCubicSegment.prototype.angleNormal = function() {
	var x = parseFloat(arguments[0]);
	var y = (typeof parseFloat(arguments[1]) == "number" && !isNaN(parseFloat(arguments[1])))?parseFloat(arguments[1]):false;
	var thetas=new Array();
	var ret=new Array();
	if    (typeof(x) == "number" && !isNaN(x)) {thetas = this.solveXforTheta(x);}
	else if (typeof(y) == "number") {thetas = this.solveYforTheta(y);}
	else {return ret;}
	for (var i=0;i<thetas.length;i++) {
		var yp=this.bezierEvalYPrimeofT(thetas[i]);
		var xp=this.bezierEvalXPrimeofT(thetas[i]);
		ret[ret.length]=Math.atan2(-xp,yp);
		}
	return ret;
	}

BezierCubicSegment.prototype.angleNormal_byTheta = function() {
	var theta = parseFloat(arguments[0]);
	var yp=this.bezierEvalYPrimeofT(theta);
	var xp=this.bezierEvalXPrimeofT(theta);
	return Math.atan2(-xp,yp);
	}
BezierCubicSegment.prototype.angleTangent_byTheta = function() {
	var theta = parseFloat(arguments[0]);
	var yp=this.bezierEvalYPrimeofT(theta);
	var xp=this.bezierEvalXPrimeofT(theta);
	return Math.atan2(yp,xp);
	}
BezierCubicSegment.prototype.f = function(x) {
	var dupseive = new Array();
	var thetas = this.solveXforTheta(x); // an array of zero to three thetas
	var ret=new Array();
	for (var i=0;i<thetas.length;i++) {
		if (!dupseive['thetas' + thetas[i]]) {
			ret[ret.length]=this.bezierEvalYofT(thetas[i]);
			 dupseive['thetas' + thetas[i]]=true;
			}
		}
	return ret;
	};
BezierCubicSegment.prototype.F = function(y) {
	var dupseive = new Array();
	var ys = this.solveYforTheta(y); // an array of zero to three thetas
	var ret=new Array();
	for (var i=0;i<ys.length;i++) {if (!dupseive['y' + ys[i]]) {ret[ret.length]=this.bezierEvalXofT(ys[i]);dupseive['y' + ys[i]]=1;}}
	return ret;
	};
BezierCubicSegment.prototype.bezierEvalXofT = function(t) {
	return (((this.A * t) + this.B) * t + this.C) * t + this.D;
	}
BezierCubicSegment.prototype.bezierEvalYofT = function(t) {
	return (((this.E * t) + this._F) * t + this.G) * t + this.H;
	}
BezierCubicSegment.prototype.point = function(t) {
	return new Array(this.bezierEvalXofT(t),this.bezierEvalYofT(t));
	}
BezierCubicSegment.prototype.bezierEvalXPrimeofT = function() {
	var t = arguments[0];
	//# x'= 3At^2+  2Bt  +     C
	//#   =(3At  +  2B)t +     C
	var ret = (this.Am3 * t  +  this.Bm2) * t + this.C;
	if (ret == 0) {ret = ((this.bezierEvalXDoublePrimeofT(t) < 0)?'-':'')+ret;} //#? is that enough? useful?
	return ret;
	}
BezierCubicSegment.prototype.bezierEvalYPrimeofT = function() {
	var t = arguments[0];
	//# y'= 3Et^2+  2Ft  +     G
	//#   =(3Et  +  2F)t +     G
	var ret = (this.Em3 * t  +  this.Fm2) * t + this.G;
	if (ret == 0) {ret = ((this.bezierEvalYDoublePrimeofT(t) < 0)?'-':'')+ret;} //#? is that enough? useful?
	return ret;
	}
BezierCubicSegment.prototype.bezierEvalXDoublePrimeofT = function() {
	var t = arguments[0];
	//# x''= 6At +  2B
	return this.Am6 * t + this.Bm2; 
	}
BezierCubicSegment.prototype.bezierEvalYDoublePrimeofT = function() {
	var t = arguments[0];
	//# y''= 6Et +  2F
	return this.Em6 * t + this.Fm2; 
	}
//BezierCubicSegment.prototype.solveXforTheta = function(x) {
BezierCubicSegment.prototype.solveXforTheta = function(x) {
	var roots = cubicformula(this.BdA,this.CdA,(this.D - x)/this.A,1);
	var ret=new Array();
	for (var i=0;i<roots.length;i++) {
		if ( 0.0000001 + 1 > new Number(roots[i]).toFixed(6) && new Number(roots[i]).toFixed(6) > 0 - 0.0000001 ) {
			ret[ret.length]=new Number(roots[i]).toFixed(6);
			}
		}
	return ret.sort(numsrt);
	}
//BezierCubicSegment.prototype.solveYforTheta = function(y) {
BezierCubicSegment.prototype.solveYforTheta = function(y) {
	var roots = cubicformula(this.FdE,this.GdE,(this.H - y)/this.E,1);
	var ret=new Array();
	for (var i=0;i<roots.length;i++) {
		if ( 0.0000001 + 1 > new Number(roots[i]).toFixed(6) && new Number(roots[i]).toFixed(6) > 0 - 0.0000001 ) {
			ret[ret.length]=new Number(roots[i]).toFixed(6);
			}
		}
	return ret.sort(numsrt);
	}
BezierCubicSegment.prototype.bezierSolveXPrimeforTheta = function(xp) {
	var roots = quadraticformula(3 * this.A,2 * this.B,this.C - xp,1);
	var ret=new Array();
	for (var i=0;i<roots.length;i++) {
		if ( 0.0000001 + 1 > new Number(roots[i]).toFixed(6) && new Number(roots[i]).toFixed(6) > 0 - 0.0000001) {
			ret[ret.length]=new Number(roots[i]).toFixed(6);
			}
		}
	return ret.sort(numsrt);
	}
BezierCubicSegment.prototype.bezierSolveYPrimeforTheta = function(yp) {
	var roots = quadraticformula(3 * this.E,2 * this._F,this.G - yp,1);
	var ret=new Array();
	for (var i=0;i<roots.length;i++) {
		if ( 0.0000001 + 1 > new Number(roots[i]).toFixed(6) && new Number(roots[i]).toFixed(6) > 0 - 0.0000001 ) {
			ret[ret.length]=new Number(roots[i]).toFixed(6);
			}
		}
	return ret.sort(numsrt);
	}
BezierCubicSegment.prototype.dimensionalStepFromTheta = function(dim,theta)  {
	var direction=arguments.length>2?arguments[2]:1;
	var pt_last = this.point(theta);
	var findnexttheta= new Function(""+
		"var pt_new = new Array(((("+this.A+" * arguments[0]) + "+this.B+") * arguments[0] + "+this.C+") * arguments[0] + "+this.D+",((("+this.E+" * arguments[0]) + "+this._F+") * arguments[0] + "+this.G+") * arguments[0] + "+this.H+");" +
		"return "+dim+" - Math.sqrt(Math.pow(pt_new[0] - "+pt_last[0]+",2) + Math.pow(pt_new[1] - "+pt_last[1]+",2));"
		);
	var result = FalsePosition(findnexttheta,new Array(theta,1),0.00001,(theta + 1)/2,'dimensionalStepFromTheta');
	if (typeof(result) == 'Object' && result[1]) {
		//#probably just reached the end
		if (Math.abs(findnexttheta(1)) < dim) {
			result[0]=1;
			}
		//#otherwise the error might be real
		}
	else {result=new Array(result,'');}
	//alert(this.point(result[0]),result[0]);
	return new Array(this.point(result[0]),result[0]);
	}

function LineSegment(p1,p2,precision) {
	//this.segType='LineSegment';
	this.p1 = new Array(parseFloat(p1[0]),parseFloat(p1[1]));
	this.p2 = new Array(parseFloat(p2[0]),parseFloat(p2[1]));
	this.precision=precision;
	var pointxssorted = new Array(this.p1[0],this.p2[0]).sort(numsrt);
	this.minx=pointxssorted[0];
	this.maxx=pointxssorted[1];
	var pointyssorted = new Array(this.p1[1],this.p2[1]).sort(numsrt);
	this.miny=pointyssorted[0];
	this.maxy=pointyssorted[1];
	this.length = Math.sqrt(Math.pow(this.maxx - this.minx,2) + Math.pow(this.maxy - this.miny,2));
	this.maxdiaglength = this.length;
	this.thetaprecision = this.precision/this.maxdiaglength;
	//this.m  = (this.p2[0] - this.p1[0] == 0)?((this.p2[1] < this.p1[1])?'-':'')+'inf':(this.p2[1] - this.p1[1])/(this.p2[0] - this.p1[0]);
	this.m  = (this.p2[1] - this.p1[1])/(this.p2[0] - this.p1[0]);
	this.b  = this.p1[1] - this.m * this.p1[0];
	this.slopeTangentVal=this.m;
	//this.slopeNormalVal =(this.slopeTangent == 0)? ((this.p2[0]<this.p1[0])?'-':'')+'inf':-1/this.slopeTangent;
	this.slopeNormalVal = isFinite(this.slopeTangent) 
	                    ? -1/this.slopeTangent 
	                    : (this.slopeTangent == Number.POSITIVE_INFINITY 
	                       ? 0  
	                       : -0 // hmm, or is the other one negative?
	                      );
	this.dx=this.p2[0] - this.p1[0];
	this.dy=this.p2[1] - this.p1[1];
	this.angleTangentVal=Math.atan2(this.dy,this.dx);
	this.angleNormalVal =Math.atan2(-this.dx,this.dy);

	}
LineSegment.prototype.segType='LineSegment';
LineSegment.prototype.getLength = function() {
	return this.length;
	}
LineSegment.prototype.precision = function() {
	if (arguments[0]) {this.precision=arguments[0];this.thetaprecision = this.precision/this.maxcoordval;}
	return this.precision;
	}
LineSegment.prototype.getRange = function() {
	return (this.minx,this.miny,this.maxx,this.maxy);
	}
LineSegment.prototype.inRange = function() {
	var coords = arguments[0];
	var xok=0;
	var yok=0;
	if (typeof parseFloat(coords[0]) == "number" && !isNaN(parseFloat(coords[0])) && this.minx <= coords[0] && this.maxx >= coords[0]) {xok=true;}
	if (typeof parseFloat(coords[1]) == "number" && !isNaN(parseFloat(coords[1])) && this.miny <= coords[1] && this.maxy >= coords[1]) {yok=true;}
	return new Array(xok,yok);
	}
LineSegment.prototype.onSegment = function() {
	var point= arguments[0];
	if (point[1] - (this.m * point[0] + this.p1[0]) < this.precision) {return true;}
	else {return false;}
	}
LineSegment.prototype.getFeet = function() {
	var x=parseFloat(arguments[0]);
	var y=parseFloat(arguments[1]);
	var feet=new Array();
	if (this.m == 0) {
		if ((this.inRange(new Array(x,false)))[0]) { feet.push(new Array(x,this.p1[1],this.solveXforTheta(x))); }
		}
	else if (!isFinite(this.m)) {
		if ((this.inRange(new Array(false,y)))[1]) { feet.push(new Array(this.p1[0],y,this.solveYforTheta(y)) );}
		}
	else {
		var intersect_x = ((this.m*this.p1[0])-(this.p1[1])+((1/this.m)*x)+(y))/(this.m + (1/this.m));
		if ((this.inRange(new Array(intersect_x,false)))[0]) {
			var intersect_y = this.f(intersect_x);
			if ((this.inRange(new Array(false,intersect_y)))[1]) {
				feet.push(new Array(intersect_x,intersect_y,this.solveXforTheta(intersect_x))); 
				}
			}
		}
	return feet;
	}
LineSegment.prototype.f = function() {
	var x = arguments[0];
	if (this.minx > x || this.maxx < x) {return;}
	if (new String(this.m).match(/(\-?)inf/)) {
		var n = eval(RegExp.$1+'1');
		var stepcount = Math.abs((this.p2[1] - this.p1[1])/this.resolution);
		var wholestepcount=Math.floor(stepcount);
		var remainder = stepcount - wholestepcount;
		var step = n * this.resolution;
		var ret=new Array();
		for (var i=0;i<=wholestepcount;i++) {ret.push(new Array(this.p1[0],this.p1[1] + step * i));}
		ret.push(new Array(this.p1[0],this.p1[1] + step * wholestepcount + step * remainder));
		return ret;
		} 
	else if (this.m == 0) {return this.p1[1];}
	else {return this.m * x + this.b;}
	}
LineSegment.prototype.F = function() {
	var y = arguments[0];
	if (this.miny > y || this.maxy < y) {return;}
	if (this.m==0) {
		var n = (this.p2[0] > this.p1[0])?1:-1;
		var stepcount = Math.abs((this.p2[0] - this.p1[0])/this.resolution);
		var wholestepcount=Math.floor(stepcount);
		var remainder = stepcount - wholestepcount;
		var step = n * this.resolution;
		var ret=new Array();
		for (var i=0;i<=wholestepcount;i++) {ret.push(new Array(this.p1[0] + step * i,this.p1[1]));}
		ret.push(new Array(this.p1[0] + step * wholestepcount + step * remainder,this.p1[1]));
		return ret;
		}
	else if (new String(this.m).match(/(\-?)inf/)) {return this.p1[0];}
	else {return (y - this.b)/this.m;}
	}
LineSegment.prototype.solveXforTheta = function() {
	return Math.abs((arguments[0] - this.p1[0])/(this.p2[0] - this.p1[0]));
	}
LineSegment.prototype.solveYforTheta = function() {
	return Math.abs((arguments[0] - this.p1[1])/(this.p2[1] - this.p1[1]));
	}
LineSegment.prototype.point = function(t) {
	var x;
	var y;
	if      (t == 0) { x=this.p1[0]; y=this.p1[1]; }
	else if (t == 1) { x=this.p2[0]; y=this.p2[1]; }
	else {
		var partdist=this.length * t;
		x=partdist * Math.cos(this.angleTangentVal) + this.p1[0];
		y=partdist * Math.sin(this.angleTangentVal) + this.p1[1];
		}
	return new Array(x,y);
	}
LineSegment.prototype.slopeTangent = function() {var ret=new Array();ret[0]=this.slopeTangentVal;return ret;}
LineSegment.prototype.slopeNormal  = function() {var ret=new Array();ret[0]=this.slopeNormalVal;return ret;}
LineSegment.prototype.angleTangent = function() {var ret=new Array();ret[0]=this.angleTangentVal;return ret;}
LineSegment.prototype.angleNormal  = function() {var ret=new Array();ret[0]=this.angleNormalVal;return ret;}
LineSegment.prototype.angleTangent_byTheta = function() {return this.angleTangentVal;}
LineSegment.prototype.angleNormal_byTheta  = function() {return this.angleNormalVal;}
LineSegment.prototype.dimensionalStepFromTheta = function(dim,theta)  {
	var direction=arguments.length>2?arguments[2]:1;
	var pt_last = this.point(theta);
	var findnexttheta= new Function(""+
		"var new_x; \n"+
		"var new_y; \n"+
		"if (arguments[0] == 0) { new_x="+this.p1[0]+"; new_y="+this.p1[1]+"; } \n"+
		"else if (arguments[0] == 1) { new_x="+this.p2[0]+"; new_y="+this.p2[1]+"; } \n"+
		"else { \n"+
		"	var partdist="+this.length+" * arguments[0]; \n"+
		"	new_x=partdist * Math.cos("+this.angleTangentVal+") + "+this.p1[0]+"; \n"+
		"	new_y=partdist * Math.sin("+this.angleTangentVal+") + "+this.p1[1]+"; \n"+
		"	} \n"+
		" return "+dim+" - "+direction+"*Math.sqrt(Math.pow(new_x - "+pt_last[0]+",2) + Math.pow(new_y - "+pt_last[1]+",2)); \n"
		);
	var result = FalsePosition(findnexttheta,new Array(theta,1),this.precision,(theta+(direction ? 1:0))/2,'dimensionalStepFromTheta');
	if (typeof(result) == 'Object' && result[1]) {
		//#probably just reached the end
		if (Math.abs(findnexttheta(direction ? 1:0)) < dim) {
			newtheta=(direction ? 1:0);
			}
		//#otherwise the error might be real
		}
	else {result=new Array(result,'');}
	return new Array(this.point(result[0]),result[0]);
	}

function EllipticalArc(p1,r,phi,large_arc_flag,sweep_flag,p2,precision) {
	this.segType='EllipticalArc';
	this.p1 = p1;
	this.r = r;
	this.rx = Math.abs(this.r[0]);
	this.ry = Math.abs(this.r[1]);
	this.phi = phi;
	this.phi = ((1000000*this.phi) % (360000000))/1000000; // angle starts as degrees, mod 360
	this.phi_radians = this.phi * (Math.PI/180);
	this.large_arc_flag = large_arc_flag;
	this.sweep_flag = sweep_flag;
	this.p2 = p2;
	this.precision=precision;
	//calc center
	//step 1: Compute (x1', y1')
	var x1prime=((this.p1[0] - this.p2[0])/2) *  Math.cos(this.phi_radians)  +  ((this.p1[1] - this.p2[1])/2) * Math.sin(this.phi_radians);
	var y1prime=((this.p1[0] - this.p2[0])/2) * -1 * Math.sin(this.phi_radians)  +  ((this.p1[1] - this.p2[1])/2) * Math.cos(this.phi_radians);
	//make sure they are large enough to make an ellipse that will reach the destination point
	var lam = Math.pow(x1prime,2)/Math.pow(this.rx,2) + Math.pow(y1prime,2)/Math.pow(this.ry,2);
	if (lam>1) {
		var sqrtlam = Math.sqrt(lam);
		this.rx*=sqrtlam+0.000000001;
		this.ry*=sqrtlam+0.000000001;
		}
	//step 2: Compute (cX ', cY  ')
	var hairy_radical = ((this.large_arc_flag == this.sweep_flag)?-1:1) * Math.sqrt(( (Math.pow(this.rx,2) * Math.pow(this.ry,2)) - (Math.pow(this.rx,2) * Math.pow(y1prime,2)) - (Math.pow(this.ry,2) * Math.pow(x1prime,2)) )/( (Math.pow(this.rx,2) * Math.pow(y1prime,2)) + (Math.pow(this.ry,2) * Math.pow(x1prime,2)) ));
	var cxprime = hairy_radical *      ((this.rx * y1prime)/this.ry);
	var cyprime = hairy_radical * -1 * ((this.ry * x1prime)/this.rx);
	//step 3: Compute (cX, cY) from (cX ', cY  ')
	this.cx = cxprime * Math.cos(this.phi) + cyprime * -1 * Math.sin(this.phi) + (parseFloat(this.p1[0]) + parseFloat(this.p2[0]))/2;
	this.cy = cxprime * Math.sin(this.phi) + cyprime *  Math.cos(this.phi) + (parseFloat(this.p1[1]) + parseFloat(this.p2[1]))/2;
	//Step 4: Compute theta1 	and 	delta-theta
	//var theta1_arccos_arg = ((y1prime - cyprime)/this.ry)/Math.sqrt(Math.pow(((y1prime - cyprime)/this.ry),2) + Math.pow(((x1prime - cxprime)/this.rx),2));
	var theta1_arccos_arg =  ((x1prime - cxprime)/this.rx)/Math.sqrt(Math.pow((x1prime - cxprime)/this.rx,2) + Math.pow((y1prime - cyprime)/this.ry,2));

//alert(''
//   +' cx::'+this.cx
//	  +' cy::'+this.cy
//	  +' phi::'+this.phi
//	   +' sweep_flag:'+this.sweep_flag
//	  +' large_arc_flag:'+this.large_arc_flag
//	  +' rx:'+this.rx
//	  +' ry:'+this.ry
//	  +' x1prime:'+x1prime
//      +' y1prime:'+y1prime
//      +' hairy_radical:'+hairy_radical
//      +' cxprime:'+cxprime
//      +' cyprime:'+cyprime
//      +' tqacoss:'+theta1_arccos_arg
//		+' phi_radians:'+this.phi_radians
//		+' sin phi_radians:'+Math.sin(this.phi_radians)
//		+' cos phi_radians:'+Math.cos(this.phi_radians)
//		);


	//this.theta1 = (((y1prime - cyprime)/this.ry < 0)?-1:1) * (Math.PI/2 - Math.atan2(theta1_arccos_arg,Math.sqrt(1 - Math.pow(theta1_arccos_arg,2))));
	var theta1sign=(((y1prime-cyprime)/this.ry)) < 0 ? -1:1;
	this.theta1 = theta1sign * (Math.PI/2 - Math.asin(theta1_arccos_arg));//hmmm, recopying latest active perl version, but this is weird with sign choosing

	//var delta_theta_arccos_arg =  (  ((-1 * x1prime - cxprime)/this.rx)*((x1prime - cxprime)/this.rx)  +  ((-1 * y1prime - cyprime)/this.ry)*((y1prime - cyprime)/this.ry)  ) / (Math.sqrt(Math.pow(((y1prime - cyprime)/this.ry),2) + Math.pow(((x1prime - cxprime)/this.rx),2)) * Math.sqrt(Math.pow(((-1 * y1prime - cyprime)/this.ry),2) + Math.pow(((-1 * x1prime - cxprime)/this.rx),2)));
	var delta_theta_arccos_arg =  (((x1prime - cxprime)/this.rx) * ((-x1prime - cxprime)/this.rx) + ((y1prime - cyprime)/this.ry) * ((-y1prime - cyprime)/this.ry))/Math.sqrt(Math.pow((x1prime - cxprime)/this.rx,2) + Math.pow((y1prime - cyprime)/this.ry,2));

	var delta_theta_sign=((((x1prime - cxprime)/this.rx) * ((-1 * y1prime - cyprime)/this.ry) ) + (((y1prime - cyprime)/this.ry) * ((-1 * x1prime - cxprime)/this.rx)) < 0)?-1:1;
	//this.delta_theta = delta_theta_sign * (Math.PI/2 - Math.atan2(delta_theta_arccos_arg,Math.sqrt(1 - Math.pow(delta_theta_arccos_arg,2))));
	this.delta_theta   = delta_theta_sign * ((Math.PI/2) - Math.asin(delta_theta_arccos_arg));

	//mod(360deg) for delta_theta
	if (Math.abs(this.delta_theta) > 2*Math.PI) {
		var div=this.delta_theta/(2*Math.PI);
		var rem=div - Math.floor(div);
		//print "DIVVING ($div - ",int($div),") = ",$rem,"\n";
		this.delta_theta = rem * (2*Math.PI);
		}

	//if (this.sweep_flag == 0 && this.delta_theta > 0) {this.delta_theta -= 2 * Math.PI;}
	//if (this.sweep_flag == 1 && this.delta_theta < 0) {this.delta_theta += 2 * Math.PI;}

	if (this.sweep_flag==1) { //alert('sweepone,dth:'+this.delta_theta);
		if (this.delta_theta < 0) {
			//if (!this.large_arc_flag) {this.delta_theta *= -1 ;}
			//else {
			//alert('changing');
				this.delta_theta += 2 * Math.PI;
			//	}
			}
		}
	else {
		if (this.delta_theta > 0) {this.delta_theta -= 2 * Math.PI;}
		//if (this.large_arc_flag && this.delta_theta > -Math.PI) {this.delta_theta = -(2*Math.PI+this.delta_theta);}
		}

	this.theta2=this.theta1 + this.delta_theta;
	//phew

//alert("delta_theta_arccos_arg:"+delta_theta_arccos_arg+
//	  "theta1_arccos_arg:"+theta1_arccos_arg+
//	  "theta1:"+this.theta1+
//	  "theta2:"+this.theta2+
//	  "delta_theta:"+this.delta_theta
//	  );

	// #calculate the foci of the ellipse
	this.f1 = this.rx > this.ry ? new Array(Math.sqrt(Math.pow(this.rx,2) - Math.pow(this.ry,2)),0) : new Array(0,Math.sqrt(Math.pow(this.ry,2) - Math.pow(this.rx,2)));
	this.f2 = this.rx > this.ry ? new Array(-1 * this.f1[0],this.f1[1]) : new Array(this.f1[0],-1 * this.f1[1]);
	// #now is a good time to calculate eccentricity, too - used in circumference and arc calculations
	this.eccentricity=this.f1/((this.rx>this.ry)?this.rx:this.ry);
	this.f1 = _rotate2d(new Array(0,0),this.f1,this.phi_radians);
	this.f1[0]+=this.cx;
	this.f1[1]+=this.cy;
	this.f2 = _rotate2d(new Array(0,0),this.f2,this.phi_radians);
	this.f2[0]+=this.cx;
	this.f2[1]+=this.cy;

	//var pointxssorted = new Array(this.cx - this.rx,this.cx + this.rx).sort(numsrt);
	//this.minx=pointxssorted[0];
	//this.maxx=pointxssorted[1];
	//var pointyssorted = new Array(this.cy - this.ry,this.cy + this.ry).sort(numsrt);
	//this.miny=pointyssorted[0];
	//this.maxy=pointyssorted[1];



	this.extremexs_is=new Array(0,1);
	var exxiszs=this.solveXPrimeforTheta(0);
	for (var i=0;i<exxiszs.length;i++) {this.extremexs_is.push(exxiszs[i]);}
	this.extremexs = new Array();
	for (var i=0;i<this.extremexs_is.length;i++) {
		//alert("exti:"+this.extremexs_is[i]+" (arctheta: "+this.normalizedThetaToArcTheta(this.extremexs_is[i])+") gives extx: "+this.evalXofTheta(this.extremexs_is[i]));
		this.extremexs.push(this.evalXofTheta(this.extremexs_is[i]));}
	var extremexs_sorted = this.extremexs.sort(numsrt);
	this.extremeys_is=new Array(0,1);
	var exyiszs=this.solveYPrimeforTheta(0);
	for (var i=0;i<exyiszs.length;i++) {this.extremeys_is.push(exyiszs[i]);}
	this.extremeys = new Array();
	for (var i=0;i<this.extremeys_is.length;i++) {
		//alert('extreme yi:'+this.extremeys_is[i]+' yeilds y:'+this.evalYofTheta(this.extremeys_is[i]));
		this.extremeys.push(this.evalYofTheta(this.extremeys_is[i]));}
	var extremeys_sorted = this.extremeys.sort(numsrt);

	this.minx = extremexs_sorted[0];
	this.maxx = extremexs_sorted[extremexs_sorted.length - 1];
	this.miny = extremeys_sorted[0];
	this.maxy = extremeys_sorted[extremeys_sorted.length - 1];
//alert(this.miny +" and max "+this.maxy );
	}
EllipticalArc.prototype.precision = function() {
	if (arguments[0]) {this.precision=arguments[0];/* this.thetaprecision = this.precision/this.maxcoordval */;}
	return this.precision;
	}
EllipticalArc.prototype.getRange = function() {
	return (this.minx,this.miny,this.maxx,this.maxy);
	}
EllipticalArc.prototype.inRange = function() {
	var coords = arguments[0];
	var xok=0;
	var yok=0;
	if (typeof parseFloat(coords[0]) == "number" && !isNaN(parseFloat(coords[0])) && this.minx <= coords[0] && this.maxx >= coords[0]) {xok=true;}
	if (typeof parseFloat(coords[1]) == "number" && !isNaN(parseFloat(coords[1])) && this.miny <= coords[1] && this.maxy >= coords[1]) {yok=true;}
	return new Array(xok,yok);
	}
EllipticalArc.prototype.arcThetaToNormalizedTheta = function(arcTheta) {
	return (arcTheta - this.theta1)/this.delta_theta;
	}
EllipticalArc.prototype.isWithinThetaRange = function(t) {
	//alert("iswin stuff: "+this.theta1 +" , "+ t +" , "+ this.theta2 +"\n");
	
	// this was all turned around and turned back again, I think,  from the perl version - need to run lots of test cases again on both

	if (this.large_arc_flag == 0) {
		if (this.sweep_flag == 0) {
			return ((t < this.theta1 && t > this.theta2) || ( t == this.theta2 || t == this.theta1)) ? 1:0;
			}
		else {
			return ((t > this.theta1 && t < this.theta2)  || ( t == this.theta2 || t == this.theta1)) ? 1:0;			
			}
		}
	else {
		if (this.sweep_flag == 0) {
			return ((t < this.theta1 && t > this.theta2) || ( t == this.theta2 || t == this.theta1)) ? 1:0;
			}
		else {
			return ((t > this.theta1 && t < this.theta2 ) || ( t == this.theta2 || t == this.theta1)) ? 1:0;
			}
		}
	}
EllipticalArc.prototype.f = function() {
	var x = arguments[0];
	var intersections=new Array();
	x-=this.cx;
	if (x < -1 * this.rx || x > this.rx) {return;}
	
	var rot_line_slope=Math.sin(Math.PI/2 - this.phi_radians)/Math.cos(Math.PI/2 - this.phi_radians);
	//print "rot_line_slope:$rot_line_slope\n";
	if (Math.abs(rot_line_slope) > 1.0 * Math.pow(10,6) || rot_line_slope == 'inf' || rot_line_slope == '-inf') {
		var y=Math.sqrt(Math.pow(this.ry,2) * (1 - (Math.pow(x,2))/(Math.pow(this.rx,2))));//vertical line. use ellipse formula to get the +/- y vals
		intersections[intersections.length]=new Array(x,y);
		intersections[intersections.length]=new Array(x,-1 * y);
		}
	else {

		var rot_line = _rotate2d(new Array(0,0),[x,0],-this.phi_radians); // point on a vertical x=C line getting tilted into ellipse frame, where the line will have a slope equal to -tan(phi)

		//print "phi : this.phi_radians\nsin(phi)/cos(phi):",sin(this.phi_radians),"/",cos(this.phi_radians),"\n";
		//print "rot line: [$rot_line->[0],$rot_line->[1]]\n";

		var a = (Math.pow(rot_line_slope,2)/Math.pow(this.ry,2)) + 1/Math.pow(this.rx,2);
		var b = ( 2 * (rot_line_slope) * (rot_line[1] - (rot_line_slope)*rot_line[0]))/Math.pow(this.ry,2);
		var c =(Math.pow(rot_line[1] - rot_line_slope * rot_line[0],2) / Math.pow(this.ry,2) ) - 1;
		//print "quad coeffs: $a, $b, $c\n";
		var xs = quadraticformula(a,b,c,1);
		//print "solution(s) from quad form:",join(",",@xs),"\n";
		for (var i=0;i<xs.length;i++) {
			var y=rot_line_slope * xs[i] + (rot_line[1] - rot_line_slope * rot_line[0]); //line formula
			intersections[intersections.length]=new Array(xs[i],y);
			}	
		}
	for (var i=0;i<intersections.length;i++) {
		var h=Math.sqrt(Math.pow(intersections[i][0],2) + Math.pow(intersections[i][1],2));
		intersections[i] = _rotate2d(new Array(0,0),intersections[i],this.phi_radians);
		intersections[i][0]+=this.cx;
		intersections[i][1]+=this.cy;
		}
	//Now check to see of those intersections are within bounds - within sweep
		
	var leg1;
	var leg2;
	if (this.large_arc_flag==0) {
		if (this.sweep_flag == 0) {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			}
		else {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			}
		}
	else {
		if (this.sweep_flag == 0) {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			}
		else {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			}		
		}
	var ret=new Array();
	for (var i=0;i<intersections.length;i++) {
		if ((this.large_arc_flag && !this.isWithinSweep(intersections[i],leg1,leg2)) || (!this.large_arc_flag && this.isWithinSweep(intersections[i],leg1,leg2))) {
			ret[ret.length] = intersections[i][1];
			}
		}
	return ret;
	}
EllipticalArc.prototype.F = function() {
	var y = arguments[0];
	var intersections=new Array();
	y-=this.cy;
	if (y < -1 * this.ry || y > this.ry) {return;}
	var rot_line_slope=Math.sin(-1 * this.phi_radians)/Math.cos(-1 * this.phi_radians);
	//print "rot_line_slope:rot_line_slope\n";
	if (Math.abs(rot_line_slope) > 1.0 * Math.pow(10,6) || rot_line_slope == 'inf' || rot_line_slope == '-inf' || Math.abs(rot_line_slope) < 1.0 * Math.pow(10,-10)) {
		var x=Math.sqrt(Math.pow(this.rx,2) * (1 - (Math.pow(y,2))/(Math.pow(this.ry,2))));//vertical line. use ellipse formula to get the +/- y vals
		intersections[intersections.length]=new Array(x,y);
		intersections[intersections.length]=new Array(-1 * x,y);
		}
	else if (Math.abs(rot_line_slope) < 1.0 * Math.pow(10,-10)) {
		var x=Math.sqrt(Math.pow(this.rx,2) * (1 - (Math.pow(y,2))/(Math.pow(this.ry,2))));//vertical line. use ellipse formula to get the +/- y vals
		intersections[intersections.length]=new Array(x,y);
		intersections[intersections.length]=new Array(-x,y);
		}
	else {

		var rot_line = _rotate2d(new Array(0,0),new Array(0,y),-this.phi_radians); // point on a vertical x=C line getting tilted into ellipse frame, where the line will have a slope equal to -tan(phi)

		//print "phi : this.phi_radians\nMath.sin(phi)/Math.cos(phi):",Math.sin(this.phi_radians),"/",Math.cos(this.phi_radians),"\n";
		//print "rot line: [$rot_line->[0],$rot_line->[1]]\n";

		var a = (1/Math.pow(this.ry,2)) + 1/(Math.pow(this.rx,2) * Math.pow(rot_line_slope,2));
		var b = (2*(rot_line[0] - (rot_line[1]/rot_line_slope)))/(Math.pow(this.rx,2) * rot_line_slope);
		var c = (Math.pow(rot_line[0] - (rot_line[1]/rot_line_slope),2) / Math.pow(this.rx,2)) - 1;
		//print "quad coeffs: $a, $b, $c\n";
		var ys = quadraticformula(a,b,c,1);
		//print "solution(s) from quad form:",join(",",@xs),"\n";
		for (var i=0;i<ys.length;i++) {
			//var x=rot_line_slope * $ys[$i] + ($rot_line->[0] - rot_line_slope * $rot_line->[1]); //line formula
			var x=((ys[i] - rot_line[1])/rot_line_slope) + rot_line[0]; //line formula
			intersections[intersections.length]=new Array(x,ys[i]);
			}	
		}
	for (var i=0;i<intersections.length;i++) {
		var h=Math.Math.sqrt(Math.pow(intersections[i][0],2) + Math.pow(intersections[i][1],2));
		intersections[i] = _rotate2d(new Array(0,0),intersections[i],this.phi_radians);
		intersections[i][0]+=this.cx;
		intersections[i][1]+=this.cy;
		}
	//Now check to see of those intersections are within bounds - within sweep
		
	var leg1;
	var leg2;
	if (this.large_arc_flag==0) {
		if (this.sweep_flag == 0) {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			}
		else {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			}
		}
	else {
		if (this.sweep_flag == 0) {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			}
		else {
			leg1=new Array(new Array(this.cx,this.cy),new Array(this.p1[0],this.p1[1]));
			leg2=new Array(new Array(this.cx,this.cy),new Array(this.p2[0],this.p2[1]));
			}		
		}
	var ret=new Array();
	for (var i=0;i<intersections.length;i++) {
		if ((this.large_arc_flag && !this.isWithinSweep(intersections[i],leg1,leg2)) || (!this.large_arc_flag && this.isWithinSweep(intersections[i],leg1,leg2))) {
			ret[ret.length] = intersections[i][0];
			}
		}
	return ret;
	}

EllipticalArc.prototype.solveYforTheta = function(y) {
	var xs = this.F(y);
	var ts = [];
	for (var i=0;i<xs.length;i++) {
		var unrot=_rotate2d([this.cx,this.cy],[xs[i],y],-this.phi_radians);
		unrot[0] -= this.cx;
		unrot[1] -= this.cy;
		//#now corresponding ellipse formulas are
		//#x=$self->{rx} * cos(theta);
		//#y=$self->{ry} * sin(theta);
		//#so that's solvable for theta, right?
		//#y=$self->{ry} * sin(theta);
		//#theta=asin(y/ry);

		var t=Math.asin(unrot[1]/this.ry);

        // perl version dev note for next one line:
        // # SIMILAR TO WHAT WE DEBUGGED FIXED FOR solveXforTheta BELOW, BUT UNTESTED/VERIFIED HERE SO FAR
        // # 5/10/2017
        if (this.sweep_flag) {t *= -1;};


		var other_t  = t + ((t>0)?-1:1) * Math.PI;
		var other_t2 = t + ((t>0)?-1:1) * 2*Math.PI;
		var other_t3 = other_t + ((other_t>0)?-1:1) * 2*Math.PI;
		ts.push(t);ts.push(other_t);ts.push(other_t2);ts.push(other_t3);
		}

    // break this perl out to for loop instead
	// return map {$self->arcThetaToNormalizedTheta($_)} grep {$self->isWithinThetaRange($_) && abs($self->evalYofTheta($_) - $y)<0.0000001} @ts;

    var ret = [];
    for(var i=0;i<ts.length;i++) {
        if (this.isWithinThetaRange(ts[i]) && Math.abs(this.evalYofTheta(ts[i]) - y)<0.0000001) {
            ret.push(this.arcThetaToNormalizedTheta(ts[i]));
            }
        }
    return ret;

	}
EllipticalArc.prototype.solveXforTheta = function(x) {
	var ys = this.f(x);
    var ts = [];
	for (var i=0;i<ys.length;i++) {
		var unrot=_rotate2d([this.cx,this.cy],[x,ys[i]],-this.phi_radians);
        unrot[0] -= this.cx;
		unrot[1] -= this.cy;
        
		//#now corresponding ellipse formulas are
		//#x=$self->{rx} * cos(theta);
		//#y=$self->{ry} * sin(theta);
		//#so that's solvable for theta, right?
		//#x=$self->{rx} * cos(theta);
		//#theta=acos(x/rx);
		//#theta=pi/2 - asin(x/rx)

        // perl note on next line
        //# DO WE NEED TO DO A DIFF CALC IF SWEEP FLAG IS ONE WAY OR THE OTHER?
        //# 5/10/2017

		var t=(Math.PI/2) - Math.asin(unrot[0]/this.rx);

        // perl note on next line
        //# NOT SURE ABOUT THIS, BUT MAYBE - looking good! for test problem case
        //# could be I only ever used this before with sweep_flag == 0 type arcs?
        //# 5/10/2017
        if (this.sweep_flag) {t *= -1;};

		var other_t  =       t + ((t      >0) ?-1:1) *   Math.PI;
		var other_t2 =       t + ((t      >0) ?-1:1) * 2*Math.PI;
		var other_t3 = other_t + ((other_t>0) ?-1:1) * 2*Math.PI;

		ts.push(t);ts.push(other_t);ts.push(other_t2);ts.push(other_t3);
		}

    // redo this perl in for loop
	//return map  {$self->arcThetaToNormalizedTheta($_)}
	//       grep {   $self->isWithinThetaRange($_) 
	//             && abs($self->evalXofArcTheta($_) - $x)<0.0000001
	//            } @ts;

    var ret = [];
    for (var i=0;i<ts.length;i++) {
        if (this.isWithinThetaRange(ts[i]) && Math.abs(this.evalXofArcTheta(ts[i]) - x)<0.0000001) {
            ret.push(this.arcThetaToNormalizedTheta(ts[i]));
            }
        }
    return ret;
	}

EllipticalArc.prototype.solveYPrimeforTheta = function(yp) {
	var yp_unrot = Math.sin(Math.atan2(yp,1) - this.phi_radians)/Math.cos(Math.atan2(yp,1) - this.phi_radians);
	var t = Math.atan2(this.ry,-this.rx * yp_unrot);
	var other_t =t + ((t>0 || t == 0)?-1:1) * Math.PI;
	var other_t2=t + ((t>0 || t == 0)?-1:1) * 2*Math.PI;
	var other_t3=other_t + ((other_t>0 || other_t == 0)?-1:1) * 2*Math.PI;
	//return map {$self->arcThetaToNormalizedTheta($_)} grep {$self->isWithinThetaRange($_)} ($t,$other_t,$other_t2,$other_t3);
	var retcand=new Array(t,other_t,other_t2,other_t3);
	var ret=new Array();
	for (var i=0;i<retcand.length;i++) {
		//alert('within test:'+this.theta1+'  >'+retcand[i]+'<    '+this.theta2+ ' ? '+this.isWithinThetaRange(retcand[i]));
		if (this.isWithinThetaRange(retcand[i])) {
			//alert('          giving i:'+this.arcThetaToNormalizedTheta(retcand[i]));
			ret.push(this.arcThetaToNormalizedTheta(retcand[i]));
			}
		}
	return ret;
	}
EllipticalArc.prototype.solveXPrimeforTheta = function(xp) {
	var xp_unrot = Math.sin(Math.atan2(xp,1) - this.phi_radians)/Math.cos(Math.atan2(xp,1) - this.phi_radians);
	var t = Math.atan2(this.ry * xp_unrot,-this.rx);
	//var other_t=t + ((t>0 || t == 0)?-1:1) * Math.PI;
	var other_t2=t + ((t>0 || t == 0)?-1:1) * 2*Math.PI;
	//var other_t3=other_t + ((other_t>0 || other_t == 0)?-1:1) * 2*Math.PI;
	//return map {$self->arcThetaToNormalizedTheta($_)} grep {$self->isWithinThetaRange($_)} ($t,$other_t,$other_t2,$other_t3);
	//var retcand=new Array(t,other_t,other_t2,other_t3);
	var retcand=new Array(t,other_t2);
	var ret=new Array();
	for (var i=0;i<retcand.length;i++) {
		if (this.isWithinThetaRange(retcand[i])) {
			//ret.push(this.arcThetaToNormalizedTheta(retcand[i]));
			ret.push(this.arcThetaToNormalizedTheta(t));
			continue;
			}
		}
	return ret;
	}

EllipticalArc.prototype.evalYofTheta = function(t) {
	var theta = this.normalizedThetaToArcTheta(t);
	if (!this.isWithinThetaRange(theta)) {return;}
	//var ret=this.rx * Math.cos(theta) * Math.sin(this.phi_radians) + this.ry * Math.sin(theta) *  Math.cos(this.phi_radians) + this.cy;
	//if (isNaN(ret)) {
	//	alert("ret:"+ ret + " from  theta:"+theta+" rx:"+this.rx+" ry:"+this.ry+" phi_radians: "+this.phi_radians+ " cy:"+this.cy);
	//	}
	return this.rx * Math.cos(theta) * Math.sin(this.phi_radians) + this.ry * Math.sin(theta) *  Math.cos(this.phi_radians) + this.cy;
	}
EllipticalArc.prototype.evalXofTheta = function(t) {
	var theta = this.normalizedThetaToArcTheta(t);
	if (!this.isWithinThetaRange(theta)) {return;}
	return this.rx * Math.cos(theta) * Math.cos(this.phi_radians) + this.ry * Math.sin(theta) * -Math.sin(this.phi_radians) + this.cx;
	}

EllipticalArc.prototype.normalizedThetaToArcTheta = function(nt) {
	//var ret=this.theta1 + this.delta_theta * nt;
	//alert("ret=" +ret +" = " +this.theta1 +" + "+this.delta_theta +" * " + nt);
	//return this.theta1 + this.delta_theta * nt;
	return this.theta1 + this.delta_theta * nt;
	}
EllipticalArc.prototype.point = function(t) {
	var at = this.normalizedThetaToArcTheta(t);
	if (!this.isWithinThetaRange(at)) {return;}
	var ct=Math.cos(at);
	var st=Math.sin(at);
	return new Array( this.rx * ct * Math.cos(this.phi_radians) + this.ry * st * -1 * Math.sin(this.phi_radians) + this.cx,this.rx * ct * Math.sin(this.phi_radians) + this.ry * st *  Math.cos(this.phi_radians) + this.cy );
	}
EllipticalArc.prototype.getLength = function() {
	// for whole length of arc, hopefully this all works
	// guess I'd like to get length of sub arcs, by passed in theta range, but haven't worked that
	// out yet in PERL version, which is the leading version of this function for now
	var res=arguments.length>0?arguments[0]:1000;
	var start_theta=arguments.length>1?arguments[1]:0;
	var end_theta=arguments.length>2?arguments[2]:1;
	
	//# if the two radii are equal, it's a circular arc
	//# (which is usually how I use this ellipse stuff)
	//# and we have 10th grade math for that
	if (this.rx == this.ry) {
		//#warn "arc length: ",($self->{rx} * $self->{delta_theta} * ($end_theta - $start_theta))," = $self->{rx} * $self->{delta_theta} * ($end_theta - $start_theta)";
		return Math.abs(this.rx * this.delta_theta * (end_theta - start_theta));
		}

//# this isn't set up yet to take arbitrary thetas for sub-lengths of ellipse arc

	var sum=0;
	var point1=this.point(start_theta);
	var point2;
	// SNIPPED comments here from PERL version, 
	// that amounted to disatisfaction with what I'm doing here
	// and IDed challenges, limitations, or some other bothersome I don't know what
	// but too fried right now to even look at this stuff, beyond translating to JS
	var thetainc=1/res;
	for (var i=0;i<res;i++) {
		point2=point1;
		//#$point1=$self->point($self->{theta1}+$i*$thetainc);
		point1=this.point(start_theta+i*thetainc);
		sum+=Math.sqrt(pow(point2[0]-point1[0],2) + pow(point2[1]-point1[1],2));	
		}
	return sum;
// or maybe it all works in perl, and now here. Look at this comment. #Those comments might not be right. I think this might work now with the alt parameterization I did for arcs. Still suspect until verified working, but glancing at it now, seems like it might be right and working.
	}
EllipticalArc.prototype.slopeTangent = function(x) {
	return 1;
	//... ? this.slopeTangent;
	}
EllipticalArc.prototype.slopeNormal  = function() {
	return 1;
	//return this.slopeNormal;
	}

EllipticalArc.prototype.angleTangent = function() {
	// #get intersect points
	var intersects = new Array();
	if (typeof(arguments[0]) == 'number')      {var ys=this.f(arguments[0]);ys.sort(numsrt);for (var i=0;i<ys.length;i++) {intersects[intersects.length]=new Array(arguments[0],ys[i]);}}
	else if (typeof(arguments[1]) == 'number') {var xs=this.F(arguments[1]);xs.sort(numsrt);for (var i=0;i<xs.length;i++) {intersects[intersects.length]=new Array(xs[i],arguments[1]);}}
	else if (typeof(arguments[2]) == 'number') {intersects[intersects.length]=this.point(arguments[2]);}
	// #then use the foci calculated in the ellipse setup
	// # and the info here: http://mysite.du.edu/~jcalvert/math/ellipse.htm
	// # to make lines and figure angles to get tangent angle...
	
	// #The tangent line at point P on the ellipse is perpendicular to the line bisecting
	// #the angle between the two lines extending from point P to the two foci of the ellipse. (So the bisector is the normal.)
	// #That angle is given by the arccosine of the dot product over the product of the magnitude of the vectors (lines) between the two lines:
	// # arccos( (line1 dot line2) / (|line1|*|line2|) ) 
	// #arccos(x) is eqivalent to pi/2 - arcsin(x)
	
	// #really you're calculating an inward pointing normal angle and adding 90 deg to get the tangent
	
	var ret = new Array;
	for (var i=0;i<intersects.length;i++) {
		var line1=new Array(intersects[i],this.f1);
		var line2=new Array(intersects[i],this.f2);
		var a1 = Math.atan2(line1[1][1] - line1[0][1],line1[1][0] - line1[0][0]);
		var a2 = Math.atan2(line2[1][1] - line2[0][1],line2[1][0] - line2[0][0]);
		// #push(@ret,($a2 + $a1)/2);
		// #print "$a1 and $a2 and minus:",($a2 - $a1),"\n";
		// #print " dot ",($pi/2 - asin((($line1->[1]->[0] - $line1->[0]->[0]) * ($line2->[1]->[0] - $line2->[0]->[0]) + ($line1->[1]->[1] - $line1->[0]->[1]) * ($line2->[1]->[1] - $line2->[0]->[1]))/(sqrt(($line1->[1]->[1] - $line1->[0]->[1])**2 + ($line1->[1]->[0] - $line1->[0]->[0])**2)*sqrt(($line2->[1]->[1] - $line2->[0]->[1])**2 + ($line2->[1]->[0] - $line2->[0]->[0])**2)))),"\n";
		ret[ret.length] =
			Math.PI/2 + // #add 90 deg from the normal angle you calculate below to get the tangent
			a1 + // #angle of the line/vector from point P on ellipse to focus 1
			((a2 - a1)>0 || (a2 - a1) == 0?1:-1) * // # hmm..., whether we add or subtract the half angle below from the line1 angle. Is this okay?
			0.5 * // # this and below calculated half the angle between the two lines
			(Math.PI/2 - 
				Math.asin(
						(
							(line1[1][0] - line1[0][0]) * (line2[1][0] - line2[0][0])  + 
							(line1[1][1] - line1[0][1]) * (line2[1][1] - line2[0][1])
						) / 
						(
							Math.sqrt(Math.pow((line1[1][1] - line1[0][1]),2) + Math.pow((line1[1][0] - line1[0][0]),2)) *
							Math.sqrt(Math.pow((line2[1][1] - line2[0][1]),2) + Math.pow((line2[1][0] - line2[0][0]),2))
						)
					)
				)
			;
		}
	var finalret=new Array();
	for (var i=0;i<ret.length;i++) {finalret[finalret.length]=angle_reduce(ret[i]);}
	return finalret;
	}
EllipticalArc.prototype.slopeTangent = function() {var ats=this.angleTangent(arguments[0],arguments[1],arguments[2]);var ret=new Array();for (var i=0;i<ats.length;i++) {ret[ret.length]= Math.sin(ats[i])/Math.cos(ats[i])     } return ret;}
EllipticalArc.prototype.slopeNormal  = function() {var ats=this.angleTangent(arguments[0],arguments[1],arguments[2]);var ret=new Array();for (var i=0;i<ats.length;i++) {ret[ret.length]= -Math.cos(ats[i])/Math.sin(ats[i])    } return ret;}
EllipticalArc.prototype.angleNormal  = function() {var ats=this.angleTangent(arguments[0],arguments[1],arguments[2]);var ret=new Array();for (var i=0;i<ats.length;i++) {ats[i]-=Math.PI/2;ret[ret.length]=angle_reduce(ats[i]);} return ret;}
EllipticalArc.prototype.isWithinSweep = function() {
	var p=arguments[0];
	var leg1=arguments[1];
	var leg2=arguments[2];
	var leftness_1 = _howleft(new Array(new Array(leg1[0][0],leg1[0][1]),new Array(leg1[1][0],leg1[1][1])),new Array(p[0],p[1]));
	var leftness_2 = _howleft(new Array(new Array(leg2[0][0],leg2[0][1]),new Array(leg2[1][0],leg2[1][1])),new Array(p[0],p[1]));
	//print "\niws: $leftness_1 * $leftness_2 <0 ?\n";
	//if ($leftness_1*$leftness_2 < 0 || $leftness_1==0 || $leftness_2==0) {return 1;}
	if ((leftness_1 < 0 && leftness_2 > 0) || leftness_1==0 || leftness_2==0) {return 1;}
	else {return 0;}
	};
EllipticalArc.prototype.angleNormal_byTheta = function(theta) {
	if (!this.isWithinThetaRange(this.normalizedThetaToArcTheta(theta))) {return;}
	ret = this.angleNormal(false,false,theta);
	return ret[0];	
	};
EllipticalArc.prototype.dimensionalStepFromTheta = function(dim,theta)  {
	var direction=arguments.length>2?arguments[2]:1;
	var pt_last = this.point(theta);
    var thys=this;
	var findnexttheta= function() {
		var pt_new  = thys.point(arguments[0]);
		return dim - direction*Math.sqrt(Math.pow(pt_new[0] - pt_last[0],2) + Math.pow(pt_new[1] - pt_last[1],2));
		};

	var result = FalsePosition(findnexttheta,new Array(theta,1),this.precision,(theta + 1)/2,'dimensionalStepFromTheta');
	if (typeof(result) == 'Object' && result[1]) {
		//#probably just reached the end
		if (Math.abs(findnexttheta(direction ? 1:0)) < dim) {
			newtheta=(direction ? 1:0);
			}
		//#otherwise the error might be real
		}
	else {result=new Array(result,'');}
	//console.log(this.point(result[0]),result[0]);
	return new Array(this.point(result[0]),result[0]);
	//return (this.point(newtheta),newtheta);
	}



//// Inheritance utility, to use for M and Z /////
function copyPrototype(descendant, parent) {  
    var sConstructor = parent.toString();  
    var aMatch = sConstructor.match( /\s*function (.*)\(/ );  
    if ( aMatch != null ) { descendant.prototype[aMatch[1]] = parent; }  
    for (var m in parent.prototype) {  
        descendant.prototype[m] = parent.prototype[m];  
    }  
};

function MoveTo(p1,p2,precision) {this.LineSegment(p1,p2,precision);}
copyPrototype(MoveTo, LineSegment);  
MoveTo.prototype.segType='MoveTo';
MoveTo.prototype.getLength=function() {return 0;}; // MOVETO DOES NOT COUNT FOR PATH LENGTH per SVG spec
function ClosePath(p1,p2,precision) {this.LineSegment(p1,p2,precision);}
copyPrototype(ClosePath, LineSegment); 
ClosePath.prototype.segType='ClosePath';



//// Utilities /////

function _howleft(line,pt) { //just copied from CAD::Calc
	var isleft = (line[1][0] - line[0][0]) * 
					    (pt[1] - line[0][1]) -
				 (line[1][1] - line[0][1]) *
				 	    (pt[0] - line[0][0]);
	return isleft;
	}
function _rotate2d(origin,point,angle) {
	var dx=(point[0]-origin[0]);
	var dy=(point[1]-origin[1]);
	//$angle = ($angle - (2*$pi)*int($angle/(2*$pi)));//usually built into the trig functions	
	//{a c-b d, a d+b c}
	return new Array(origin[0] + (dx*Math.cos(angle) - dy*Math.sin(angle)),origin[1] + (dx*Math.sin(angle) + dy*Math.cos(angle)));
	}






//// Cubic Formula Solver /////
// test case:
//var test = cubicformula(-11,49,-75);
//alert(test.join(', ') + '\nvs 3, 4+3i, 4-3i\n');


function cubicformula(A,B,C) {
				//Originally from Stephen R. Schmitt http://home.att.net/~srschmitt/script_exact_cubic.html
				//#translated by Mike Sheldrake into PERL, and modified
				//then translated back to javascript by Mike Sheldrake
	var onlyreal = (arguments.length > 3)? arguments[3]: 0 ;
    var Im = cubic(A,B,C);
    if (Im[0] == '0.0') {                          // real roots
//alert('all real: '+Im[1]+','+Im[2]+','+Im[3]);
        return new Array(Im[1].toFixed(6),Im[2].toFixed(6),Im[3].toFixed(6));
	    }
    else {                                          // real and complex pair
//alert('some real: '+Im[1]+', '+Im[2]+' + '+Im[0]+'i, '+Im[3]+' - '+Im[0]+'i');
    	if (onlyreal) {
			var ret = new Array();
			ret[0]=Im[1].toFixed(6);
			return ret;
			} 
		else {
			return new Array(Im[1].toFixed(6),Im[2].toFixed(6)+' + '+Im[0].toFixed(6)+'i',Im[3].toFixed(6)+' - '+Im[0].toFixed(6)+'i');
			}
	    }
	}
// compute real or complex roots of cubic polynomial
function cubic(A,B,C) {
	var Q;
	var R;
	var D;
	var S;
	var T;
	var Im;
	var X1;
	var X2;
	var X3;

    Ad3 = A/3;
    R = (9*A*B - 27*C - 2*Math.pow(A,3))/54;
    D = Math.pow(Q,3) + Math.pow(R,2);                // polynomial discriminant

    if (D >= 0) {                                     // complex or duplicate roots
    
        preS = R + Math.sqrt(D);
        preT = R - Math.sqrt(D);
        S = (preS < 0 ? -1:1) * (Math.pow(Math.abs(preS),(1/3)));
        T = (preT < 0 ? -1:1) * (Math.pow(Math.abs(preT),(1/3)));
        SpT = S + T;

        X1 = -Ad3 + SpT;                    // real root
        X2 = -Ad3 - SpT/2;                  // real part of complex root
        X3 = X2;                                      // real part of complex root
        Im = Math.abs(Math.sqrt(3)*SpT/2);        // complex part of root pair
	    }
    else {                                            // distinct real roots
        Q = (3*B - Math.pow(A,2))/9;
        twosqrtnegQ = 2*Math.sqrt(-1 * Q);
        
        var th = Math.acos(R/Math.sqrt(-1 * (Math.pow(Q,3))));
		//Perl way to get acos without using problematic Math::Trig package:
		//var toAcos = R / Math.sqrt( -1 * (Math.pow(Q,3)) );
		//var th = Math.atan(Math.sqrt(1 - toAcos * toAcos)/toAcos);
        
        X1 = twosqrtnegQ * Math.cos( th             /3) - Ad3;
        X2 = twosqrtnegQ * Math.cos((th + 2*Math.PI)/3) - Ad3;
        X3 = twosqrtnegQ * Math.cos((th + 4*Math.PI)/3) - Ad3;
        Im = '0.0';
	    }
	
    return new Array(Im,X1,X2,X3);                     // if Im == 0.0 all are real roots
	}

/////// Quadratic Solver //////////
function quadraticformula(a,b,c) {
	var realonly = (arguments.length > 3)? arguments[3]: 0 ;
	var im = '';
	if (Math.abs(a) < 0.000000001) {var ret = new Array();ret[0]=((-1*b)/a - c);return ret;} // just a line
	if (Math.abs(c) < 0.000000001) {return new Array(0, (-1*b)/a);}     // because it degrades to x(ax+b)
	var dis = b*b - a * 4 * c;                     // discriminant
	var a2  = 2*a;                                 // 2a
	var nb2a= -1 * b/a2;                           // -b/2a
//	if ($dis - 0 < 0.00000000000001) {return $nb2a;}// one root
	if (dis < 0 && !realonly) {                    // imaginary roots
		var imaginary = (Math.sqrt(a * 4 * c - b*b)/a2) +'i';
		return new Array(nb2a+' + '+imaginary,nb2a+' - '+imaginary);
		}
	else if (dis >= 0) {                                          // one (double) or two real roots
		var rightside = Math.sqrt(dis)/a2;
		return new Array(nb2a + rightside,nb2a - rightside);
		}
	else {return new Array();}
	}


///////////////// Root Finding ///////////
function FalsePosition(F,bounds,precision,x) {
	var f = F(x);
	var x0=bounds[0];
	var f0=F(x0);
	if (Math.abs(f) < precision) {return x;}
	if (Math.abs(f0) < precision) {return x0;}
	if ((f0>0 && f>0) || (f0<0 && f<0)) {
		x0=bounds[1];
		f0=F(x0);
		if (Math.abs(f0) < precision) {return x0;}
		}
	if ((f0>0 && f>0) || (f0<0 && f<0)) {
		return (undefined,'can\'t do False Position method with those bounds ['+bounds[0]+','+bounds[1]+"]\n  f("+bounds[0]+') = '+F(bounds[0])+"\n  f("+bounds[1]+') = '+F(bounds[1])+"\n    and/or that guess\n  f("+x+') = '+F(x)+"\nwith that precision: "+precision+"\n");
		}
	var limit=0;
	while (limit++<2000) {//#method of false position (bracketing root finding algorithm)
		var prex = x;
		var denom= f - f0;
		//if (denom==0 ) {f0=F(Math::BigFloat->new($x0));$f=&{$F}(Math::BigFloat->new($prex));$denom =  $f - $f0 ;}
		x= x0 - (f0 * ((x - x0)/denom));
		f=F(x);
		if (Math.abs(f) < precision) {return x;}
		if ((f0>0 && f>0) || (f0<0 && f<0)) {x0 = prex;f0=F(x0);} //#retain last guess that gave function opposite sign
		}
	return (undefined,'too many iterations in False Position method');
	}

BrentsMethodMaxIter = 50000;

function BrentsMethod(withobj,F,bounds,e,guess,callername) {
	var a=bounds[0];
	var b=bounds[1];
	var lowbound  = (bounds[0]<bounds[1])?bounds[0]:bounds[1];
	var highbound = (bounds[0]>bounds[1])?bounds[0]:bounds[1];
	var c=a;
	var fofa=F.call(withobj,a);
	var fofb=F.call(withobj,b);
	var fofc=fofa;
	var fofd;
	var m=(a + b)/2;
	var d = 0;
	var lastd='';
	if (Math.abs(fofa) < e || Math.abs(fofa) == e) {return [a,null];}
	if (Math.abs(fofb) < e || Math.abs(fofb) == e) {return [b,null];}
	if (fofa * fofb > 0) {return [null,'f(a) and f(b) have same sign: ' + " f("+a+"): "+fofa+" vs  f("+b+"): "+fofb+"\n"]}
	for (var i=1;i<=BrentsMethodMaxIter;i++) {
		if (fofa != fofc && fofb != fofc) { //# inverse quadratic interpolation - Lagrange interpolating polynomial of degree 2
			d = (a * fofb * fofc)/((fofa - fofb) * (fofa - fofc)) + 
				(b * fofa * fofc)/((fofb - fofa) * (fofb - fofc)) + 
				(c * fofa * fofb)/((fofc - fofa) * (fofc - fofb));
			}
		else { //# linear interpolation - secant method
			d = b - ( fofb * ( (b - a)/(fofb - fofa) ) );
			}

		if ( (m < b && (d < m || d > b)) ||
			 (m > b && (d > m || d < b)) ) { //# bisection
			d=m;
			}
		fofd = F.call(withobj,d);
		if (Math.abs(fofd) < e || Math.abs(fofd) == e) {return new Array(d,false);}
		if (lastd == d) {
			//in PERL, turned up precision. Here, either bail now or let it iterate out
			return new Array(null,'in adequate precision in Brent\'s method');
			}
		//#this retains the last point of opposite sign
		//#which keeps root bracketed while shrinking size of bracket
		if ((fofb > 0 && fofd < 0) || (fofb < 0 && fofd > 0)) {    a=b;    fofa=fofb;    }
		c=b;
		b=d;
		if (a == c) {fofc=fofa;}
		else {fofc=fofb;}
		fofb=fofd;
		m = (a + b) / 2;
		lastd=d;
		}
	throw "\nTOO MANY!! too many iterations in Brent\'s method\n\n";
	return new Array(null,'too many iterations in Brent\'s method');
	}