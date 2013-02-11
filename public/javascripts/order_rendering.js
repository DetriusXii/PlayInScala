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

function getLineOption(srcLocationName, 
	dstLocationName, empireStrokeStyleName) {
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

function getSupportHoldGraphic(srcPoint, 
		dstPoint, radius, empireStrokeStyleName) {
	var g = document.createElementNS(svgNamespace, "g");
	
	var shadowPolygon = document.createElementNS(svgNamespace, "polygon");
	var coloredPolygon = document.createElementNS(svgNamespace, "polygon");
	var shadowLine = document.createElementNS(svgNamespace, "line");
	var coloredLine = document.createElementNS(svgNamespace, "line");
	
	var NUM_OCTAGON_POINTS = 8;
	var STARTING_PHASE = Math.PI / 8;
	
	var points = getPointListAroundCentreForNVertexPolygon(dstPoint, 
			radius, NUM_OCTAGON_POINTS, STARTING_PHASE);
	
	var formattedPoints = points.map(function(point) {
		return point.x + "," + point.y; 
	});
	
	var pointList = formattedPoints.join(" ");
	var intersectionPointOption = 
		getIntersectionPointBetweenLineAndPolygon(srcPoint, 
			dstPoint, radius, NUM_OCTAGON_POINTS, STARTING_PHASE);
	
	
	shadowPolygon.setAttribute("class", "shadowdash");
	shadowPolygon.setAttribute("points", pointList);
	
	coloredPolygon.setAttribute("class", 
			empireStrokeStyleName + " supportorder");
	coloredPolygon.setAttribute("points", pointList);
	
	
	intersectionPointOption.map(function(iPoint) {
		shadowLine.setAttribute("class", "shadowdash");
		shadowLine.setAttribute("x1", srcPoint.x);
		shadowLine.setAttribute("x2", iPoint.x);
		shadowLine.setAttribute("y1", srcPoint.y);
		shadowLine.setAttribute("y2", iPoint.y);
		
		coloredLine.setAttribute("class", 
				empireStrokeStyleName + " supportorder");
		coloredLine.setAttribute("x1", srcPoint.x);
		coloredLine.setAttribute("x2", iPoint.x);
		coloredLine.setAttribute("y1", srcPoint.y);
		coloredLine.setAttribute("y2", iPoint.y);
		
		g.appendChild(shadowLine);
		g.appendChild(coloredLine);
		
		return null;
	});
	
	g.appendChild(shadowPolygon);
	g.appendChild(coloredPolygon);
	
	return g;
}

function getPointListAroundCentreForNVertexPolygon(centrePoint, 
		radius, numVertices, startingPhase) {
	var phaseIncrement = 2*Math.PI/numVertices;
	var points = [];
	
	for (var i = 0; i < numVertices; i++) {
		var phase = i*phaseIncrement + startingPhase;
		var r_x = radius*Math.cos(phase);
		var r_y = radius*Math.sin(phase);
		
		var xyPoint = new Point(centrePoint.x + r_x, centrePoint.y + r_y);
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
	
	return phaseRangeOption.bind(function(phaseRange) {
		return getIntersectionBetweenLines(srcPoint, dstPoint, 
				phaseRange.startPoint, phaseRange.endPoint);
	});
}

function getIntersectionBetweenLines(s1, s2, d1, d2) {
	var slopeSOption = getSlopeOption(s1, s2);
	var slopeDOption = getSlopeOption(d1, d2);
	var bInterceptSOption = getBInterceptOption(s1, s2);
	var bInterceptDOption = getBInterceptOption(d1, d2);
	
	if (slopeSOption instanceof None && slopeDOption instanceof Some) {
		var xIntersect = s1.x;
		var yIntersectOption = slopeDOption.bind(function(slopeD) {
			return bInterceptDOption.map(function(bInterceptD) {
				return slopeD*xIntersect + bInterceptD;
			});
		});
		
		return yIntersectOption.map(function(yIntersect) {
			return new Point(xIntersect, yIntersect);
		})
	} else if (slopeSOption instanceof Some && 
			slopeDOption instanceof None) {
		var xIntersect = d1.x;
		var yIntersectOption = slopeSOption.bind(function(slopeS) {
			return bInterceptSOption.map(function(bInterceptS) {
				return slopeS*xIntersect + bInterceptS;
			});
		})
		
		return yIntersectOption.map(function(yIntersect) {
			return new Point(xIntersect, yIntersect);
		});
	} else if (slopeSOption instanceof None && 
			slopeDOption instanceof None) {
		return new None();
	} else {
		return slopeSOption.bind(function(slopeS) {
			return slopeDOption.bind(function(slopeD) {
				return bInterceptSOption.bind(function(bInterceptS) {
					return bInterceptDOption.bind(function(bInterceptD) {
						if (slopeS === slopeD) {
							return new None();
						} else {
							var bDiff = bInterceptS - bInterceptD;
							var slopeDiff = slopeD - slopeS;
							var xIntersect = bDiff / slopeDiff;
							var yIntersection = 
								slopeS*xIntersect + bInterceptS;
							return new 
								Some(new Point(xIntersect, yIntersect));
						}
					});
				});
			});
		});
	}
}

function getBInterceptOption(srcPoint, dstPoint) {
	var xDist = dstPoint.x - srcPoint.x;
	var yDist = dstPoint.y - srcPoint.y;
	
	if (xDist === 0.0) {
		return new None();
	} else {
		var slopeOption = getSlopeOption(srcPoint, dstPoint);
		return slopeOption.map(function(slope) {
			return srcPoint.y - slope*srcPoint.x;
		});
	}
}

function getSlopeOption(srcPoint, dstPoint) {
	var xDist = dstPoint.x - srcPoint.x;
	var yDist = dstPoint.y - srcPoint.y;
	
	if (xDist === 0.0) {
		return new None();
	} else {
		return new Some(yDist/xDist);
	}
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


function Point(x, y) {
	this.x = x;
	this.y = y;
}

function getSupportOrderOption(srcLocationName, 
		dstLocationName, 
		empireStrokeStyleName) {
	var srcAlternateNameOption = getUsedAlternateName(srcLocationName);
	var dstAlternateNameOption = getUsedAlternateName(dstLocationName);
	
	return srcAlternateNameOption.bind(function(srcAlternateName) {
		return dstAlternateNameOption.map(function(dstAlternateName) {
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
			
			var srcUnit = srcProvince.getElementsByTagName("jdipns:unit")[0];
			var dstUnit = dstProvince.getElementsByTagName("jdipns:unit")[0];
			
			var x1 = parseInt(srcUnit.getAttribute("x"));
			var x2 = parseInt(dstUnit.getAttribute("x"));
			var y1 = parseInt(srcUnit.getAttribute("y"));
			var y2 = parseInt(dstUnit.getAttribute("y"));
			
			var srcPoint = new Point(x1, y1);
			var dstPoint = new Point(x2, y2);
			
			return getSupportHoldGraphic(srcPoint, 
						dstPoint, 30, empireStrokeStyleName)
			
		});
	});
}

function getProvinceTripleTupleOption(filteredProvinceElements, 
		srcName, supportName, targetName) {
	var srcProvinceOption = filteredProvinceElements.find(function(prov) {
		return prov.getAttribute("name") === srcName;
	});
	var sptProvinceOption = filteredProvinceElements.find(function(prov) {
		return prov.getAttribute("name") === supportName;
	});
	var tgtProvinceOption = filteredProvinceElements.find(function(prov) {
		return prov.getAttribute("name") === targetName;
	});
	
	return srcProvinceOption.bind(function(srcProv) {
		return sptProvinceOption.bind(function(sptProv) {
			return tgtProvinceOption.map(function(tgtProvince) {
				return {a: srcProv, b: sptProv, c: tgtProv};
			});
		});
	});
}

function getSupportMoveOrderOption(srcLocationName, 
		supportUnitLocationName, targetLocationName, empireStrokeStyleName) {
	var srcAlternateNameOption = getUsedAlternateName(srcLocationName)
	var supportAlternateNameOption = 
		getUsedAlternateName(supportUnitLocationName)
	var targetAlternateNameOption = getUsedAlternateName(targetLocationName)
	
	return srcAlternateNameOption.bind(function(srcName) {
		return supportAlternateNameOption.bind(function(supportName) {
			return targetAlternateNameOption.bind(function(targetName) {
				var provinceElements = 
					getArrayFromNodeList(
							document.getElementsByTagName("jdipns:province"))
				
				var filteredProvinceElements = 
					provinceElements.filter(function(provElem) {
						return provElem.getAttribute("name") === srcName ||
							provElem.getAttribute("name") === supportName ||
							provElem.getAttribute("name") === targetName;
					});
				var tupleOption =
					getProvinceTripleTupleOption(filteredProvinceElements,
						srcName, supportName, targetName);
				
				return tupleOption.map(function(tuple) {
					var srcProv = tuple.a;
					var sptProv = tuple.b;
					var tgtProv = tuple.c;
					
					var srcUnit =
						srcProv.getElementsByTagName("jdipns:unit")[0];
					var sptUnit =
						sptProv.getElementsByTagName("jdipns:unit")[0];
					var tgtUnit =
						tgtProv.getElementsByTagName("jdipns:unit")[0];
					
					var srxX = srcUnit.getAttribute("x");
					var srcY = srcUnit.getAttribute("y");
					var sptX = sptUnit.getAttribute("x");
					var sptY = sptUnit.getAttribute("y");
					var tgtX = tgtUnit.getAttribute("x");
					var tgtY = tgtUnit.getAttribute("y");
					
					var g = document.createElementNS(svgNamespace, "g");
					
					var shadowPath = 
						document.createElementNS(svgNamespace, "path");
					var coloredPath =
						document.createElementNS(svgNamespace, "path");
					
					var srcCoordinate = srcX + "," + srcY;
					var sptCoordinate = sptX + "," + sptY;
					var tgtCoordinate = tgtX + "," + tgtY;
					var d = "M " + srcCoordinate + " C " +
						sptCoordinate + " " + sptCoordinate + " " + 
						tgtCoordinate;
					
					shadowPath.setAttribute("d", d);
					shadowPath.setAttribute("class", supportorder);
					
					coloredPath.setAttribute("d", d);
					coloredPath.setAttribute("class", empireStrokeStyleName +
							" supportorder");
					coloredPath.setAttribute("marker-end", "url(#arrow)");
					
					g.appendChild(shadowPath);
					g.appendChild(coloredPath);
					
					return g;
				});
			});
		});
	});
}
