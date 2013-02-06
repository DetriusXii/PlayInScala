function getUsedAlternateName(srcLocationName) {
	var splitName = srcLocationName.split("-")
	var provinceName = splitName[0]
	
	var upnNames = getUniqueProvinceNames().filter(function(upn) {
		return upn.provinceName === provinceName
	}).map(function(upn) {
		return upn.alternateName;
	});
	
	var provinceElements = 
		getArrayFromNodeList(document.getElementsByTagName("jdipns:province"));
	
	
	var usedUpnNames = upnNames.filter(function(alternateName) {
		return provinceElements.some(function(elem) {
			return elem.getAttribute("name") === alternateName;
		});
	});
	
	if (usedUpnNames.length > 0) {
		return new Some(usedUpnNames[0]);
	} else {
		return new None;
	}
}

function getArrayFromNodeList(nodeList) {
	var array = [];
	
	for (var i = 0; i < nodeList.length; i++) array.push(nodeList[i]);
	
	return array;
}

function getProvinceTuple(provinceElems, srcName) {
	if (provinceElems[0].getAttribute("name") === srcName) {
		return {a: provinceElems[0], b: provinceElems[1]};
	} else {
		return {a: provinceElems[1], b: provinceElems[0]};
	}
}

function getLine(srcLocationName, 
	dstLocationName, strokeWidth, empireStrokeStyleName) {
	var srcAlternateName = getUsedAlternateName(srcLocationName);
	var dstAlternateName = getUsedAlternateName(dstLocationName);
	var svgNamespace = "http://www.w3.org/2000/svg";
	
	return srcAlternateName.bind(function(srcName) {
		return dstAlternateName.map(function(dstName) {
			var provinceElements = 
				document.getElementsByTagName("jdipns:province");
			var filterProvinceElements = $(provinceElements).filter(function() {
				return this.getAttribute("name") === srcName || 
						this.getAttribute("name") === dstName;
			}).get();
			
			var tuple = getProvinceTuple(filterProvinceElements, srcName);
			var srcProvince = tuple.a;
			var dstProvince = tuple.b;
			
			var srcUnit = srcProvince.getElementsByTagName("jdipns:unit")[0];
			var dstUnit = dstProvince.getElementsByTagName("jdipns:unit")[0];
			
			var g = document.createElementNS(svgNamespace, "g");
			
			var x1 = srcUnit.getAttribute("x");
			var x2 = dstUnit.getAttribute("x");
			var y1 = srcUnit.getAttribute("y");
			var y2 = dstUnit.getAttribute("y");
			
			var shadowLine = document.createElementNS(svgNamespace, "line");
			shadowLine.setAttribute("x1", x1);
			shadowLine.setAttribute("x2", x2);
			shadowLine.setAttribute("y1", y1);
			shadowLine.setAttribute("y2", y2);
			shadowLine.setAttribute("class", "shadoworder");
			
			
			var svgLine = 
				document.createElementNS(svgNamespace, "line");
			svgLine.setAttribute("x1", x1);
			svgLine.setAttribute("x2", x2);
			svgLine.setAttribute("y1", y1);
			svgLine.setAttribute("y2", y2);
			svgLine.setAttribute("class", 
					empireStrokeStyleName + " defaultorder");
			svgLine.setAttribute("marker-end", "url(#arrow)");
			
			g.appendChild(shadowLine);
			g.appendChild(svgLine);
			
			return g;
		});
	});
}

function getDistance(srcUnit, dstUnit) {
	var xcompSquare = Math.pow(dstUnit.x - srcUnit.x, 2);
	var ycompSquare = Math.pow(dstUnit.y - srcUnit.y, 2);

	return Math.sqrt(xcompSquare + ycompSquare);
}

function getPhiAngle(srcUnit, dstUnit) {
	var xDist = dstUnit.x - srcUnit.x;
	var yDist = dstUnit.y - srcUnit.y;
	
	if (xDist == 0.0 && yDist == 0.0) {
		return new None();
	} else {
		return new Some(Math.atan2(yDist, xDist));
	}
}


