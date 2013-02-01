function getLine(srcLocationName, dstLocationName, strokeWidth) {
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
	
	var svgLine = document.createElement("line");
	svgLine.setAttribute("x1", srcUnit.getAttribute("x"));
	svgLine.setAttribute("x2", dstUnit.getAttribute("x"));
	svgLine.setAttribute("y1", srcUnit.getAttribute("y"));
	svgLine.setAttribute("y2", dstUnit.getAttribute("y"));
	svgLine.setAttribute("stroke-width", strokeWidth);

	return svgLine;
}

function getDistance(srcUnit, dstUnit) {
	var xcompSquare = Math.pow(dstUnit.x - srcUnit.x, 2);
	var ycompSquare = Math.pow(dstUnit.y - srcUnit.y, 2);

	return Math.sqrt(xcompSquare + ycompSquare);
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


