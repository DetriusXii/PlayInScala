function getLine(srcLocationName, dstLocationName) {
	var provinceElements = 
		document.getElementsByTagName("jdipns:province");
	var filterProvinceElements = provinceElements.filter(function(element) {
		return element.name === srcLocationName || 
			element.name === dstLocationName;
	});
	
	
	function getProvinceTuple() {
		if (filterProvinceElements[0].name === srcLocationName) {
			return {a: filterProvinceElements[0], b: filterProvinceElements[1]};
		} else {
			return {a: filterProvinceElements[1], b: filterProvinceElements[2]};
		}
	}
	
	var tuple = getProvinceTuple();
	var srcProvince = tuple.a;
	var dstProvince = tuple.b;
	
	var srcUnit = srcProvince.getElementsByTagName("jdipns:unit");
	var dstUnit = dstProvince.getElementsByTagName("jdipns:unit");
	
	document.createElement(
}

function getDistance(srcUnit, dstUnit) {
	var xcompSquare = Math.pow(dstUnit.x - srcUnit.x, 2);
	var ycompSquare = Math.pow(dstUnit.y - srcUnit.y, 2);

	return Math.sqrt(xcompSquare + ycompSquare);
}

function NotOptionException() {}

function Option() {
	this.map = function(mappingFunction) {
		if (this instanceof Some) {
			return new Some(mappingFunction(this.value));
		} else {
			return new None;
		}
	}
	
	this.bind = function(bindingFunction) {
		if (this instanceof Some) {
			var newMonad = bindingFunction(this.value);
			
			if (newMonad instanceof Option) {
				return newMonad;
			} else {
				throw new NotOptionException();
			}
		} else {
			return new None();
		}
	}
}

Option.prototype = {
	pure: function(value) {
		return new Some(value);
	}
}

function Some(value) {
	Option.call(this);
	this.value = value;
}

Some.prototype = Object.create(Option.prototype);
None.prototype = Object.create(Option.prototype);

function None() {
	Option.call(this);
}

function getPhiAngle(srcUnit, dstUnit) {
	var xDist = dstUnit.x - srcUnit.x;
	var yDist = dstUnit.y - srcUnit.y;
	
	if (xDist == 0.0 and yDist == 0.0) {
		return new None();
	} else {
		return new Some(Math.atan2(yDist, xDist));
	}
}


