var svgNamespace = "http://www.w3.org/2000/svg";

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
		if (splitName.length > 1) {
			return new Some(usedUpnNames[0] + "-" + splitName[1]);
		} else {
			return new Some(usedUpnNames[0]);
		}
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

function getDistance(srcPoint, dstPoint) {
	var xcompSquare = Math.pow(dstPoint.x - srcPoint.x, 2);
	var ycompSquare = Math.pow(dstPoint.y - srcPoint.y, 2);

	return Math.sqrt(xcompSquare + ycompSquare);
}

function getPhiAngle(srcPoint, dstPoint) {
	var xDist = dstPoint.x - srcPoint.x;
	var yDist = dstPoint.y - srcPoint.y;
	
	if (xDist == 0.0 && yDist == 0.0) {
		return new None();
	} else {
		return new Some(Math.atan2(yDist, xDist));
	}
}

function getOctagonAroundCentre(centre, radius, empireStrokeStyleName) {
	var g = document.createElementNS(svgNamespace, "g");
	
	var shadowPolygon = document.createElementNS(svgNamespace, "polygon");
	var coloredPolygon = document.createElementNS(svgNamespace, "polygon");
	
	var NUM_OCTAGON_POINTS = 8;
	var STARTING_PHASE = Math.PI / 8;
	
	var points = getPointListAroundCentreForNVertexPolygon(centre, 
			radius, NUM_OCTAGON_POINTS, STARTING_PHASE);
	
	var formattedPoints = points.map(function(point) {
		return point.x + "," point.y; 
	});
	
	var pointList = formattedPoints.join(" ");
	
	shadowPolygon.setAttribute("class", "shadowdash");
	shadowPolygon.setAttribute("points", pointList);
	
	coloredPolygon.setAttribute("class", 
			empireStrokeStyleName + " supportorder");
	coloredPolygon.setAttribute("points", pointList);
	
	g.appendChild(shadowPolygon);
	g.appendChild(coloredPolygon);
	
	return g;
}

function getPointListAroundCentreForNVertexPolygon(centrePoint, 
		radius, numVertices, startingPhase) {
	var phaseIncrement = 2*Math.PI/numVertices;
	var points = [];
	
	for (var i = 0; i < (numVertices + 1); i++) {
		var phase = i*phaseIncrement + startingPhase;
		var r_x = radius*Math.cos(phase);
		var r_y = radius*Math.sin(phase);
		
		var xyPoint = new Point(center.x + r_x, center.y + r_y);
		points.push(xyPoint);
	}
	
	return points;
}

function getIntersectionPointBetweenLineAndPolygon(srcPoint, 
		dstPoint, radius, numVertices, startingPhase) {
	
	var points = getPointListAroundCentreForNVertexPolygon(dstPoint, 
			radius, numVertices, startingPhase);
	
	var phaseOptions = points.map(function(point) {
		return getPhiAngle(dstPoint, point);
	});
	var phaseRanges = phaseOptions.map(function(phase, idx) {
		var secondPhase = phaseOptions[(idx + 1) % numVertices];
		return {startPoint: points[idx], 
			endPoint: points[(idx + 1) % numVertices],
			startPhase: phase, 
			endPhase: secondPhase};
	});
	
	var phaseFromDstToSrc = getPhiAngle(dstPoint, srcPoint);
	var phaseRangeOption = phaseFromDstToSrc.bind(function(phase) {
		return phaseRanges.find(function(phaseRange, idx) {
			var isBetweenOption = phaseRange.startPhase.bind(function(sPhase) {
				return phaseRange.endPhase.map(function(ePhase) {
					return isInBetweenPhase(phase, sPhase, ePhase);
				});
			});
			
			if (isBetweenOption instanceof Some) {
				return isBetweenOption.value;
			} else {
				return false;
			}
		});
	})
	
	phaseRangeOption.map(function(phaseRange) {
		
	});
}

function getIntersectionBetweenLines(s1, s2, d1, d2) {
	
}

function isInBetweenPhase(phase, startPhase, endPhase) {
	var nStartPhase = (2*Math.PI + startPhase) % (2*Math.PI);
	var nEndPhase = (2*Math.PI + endPhase) % (2*Math.PI);
	var nPhase = (2*Math.PI + phase) % (2*Math.PI);
	
	if (nStartPhase < nEndPhase && 
			nStartPhase <= nPhase && nPhase < nEndPhase) {
		return true;
	} else if (nStartPhase > nEndPhase && 
			nStartPhase <= nPhase ) {
		return true;
	} else if (nStartPhase > nEndPhase &&
			nPhase < nEndPhase) {
		return true;
	} else {
		return false;
	}
}

function getLinePhaseAngle() {
	
}

function Point(x, y) {
	this.x = x;
	this.y = y;
}


function getSupportOrder(srcLocationName, 
		dstLocationName, 
		empireStrokeStyleName) {
	var srcAlternateNameOption = getUsedAlternateName(srcLocationName);
	var dstAlternateNameOption = getUsedAlternateName(dstLocationName);
	
	return srcAlternateNameOption.bind(function(srcAlternateName) {
		return dstAlternateNameOption.bind(function(dstAlternateName) {
			var provinceElements = 
				getArrayFromNodeList(
						document.getElementsByTagName("jdipns:province"));
			var filteredProvinceElements = 
				provinceElements.filter(function(provElem) {
					var nameAttribute = provElem.getAttribute("name");
					return nameAttribute === srcAlternateName || 
						nameAttribute === dstAlternateName;
				});
			
			var tuple = getProvinceTuple(filteredProvinceElements, 
					srcAlternateName);
			var srcProvince = tuple.a;
			var dstProvince = tuple.b;
			
			var srcUnit = srcProvince.getElementsByTagName("jdipns:unit");
			var dstUnit = dstProvince.getElementsByTagName("jdipns:unit");
			
			var x1 = srcUnit.getAttribute("x");
			var x2 = dstUnit.getAttribute("x");
			var y1 = srcUnit.getAttribute("y");
			var y2 = dstUnit.getAttribute("y");
			
			var dstPoint = new Point(x2, y2);
			
			var octagonAroundCentre = 
				getOctagonAroundCentre(dstPoint, )
			
		});
	});
}

