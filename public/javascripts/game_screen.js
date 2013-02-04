function getUniqueProvinceNames() {
	return @GameScreenController.
}

function getBasicHeading(id) {
	return "<tr class='heading " + id + "'>" + 
		"<td></td><td></td><td></td><td>Target</td><td></td></tr>";
}

function getAdvancedHeading(id) {
	return "<tr class='heading " + id + "'>" +
		"<td></td><td></td><td></td><td>Source</td><td>Target</td></tr>"
}

function getPotentialMoveOrders() {
	return @Html(potentialMoveOrders.toString);
}

function getPotentialSupportHoldOrders() {
	return @Html(potentialSupportHoldOrders.toString);
}

function getPotentialSupportMoveOrders() {
	return @Html(potentialSupportMoveOrders.toString);
}

function getPotentialConvoyOrders() {
	return @Html(potentialConvoyOrders.toString);
}

function createClassName(name) {
	return "." + name;
}

function getBasicOrdersForId(id, orders) {
	return orders.filter(function(order) {
		return order.@GameScreenController.DIPLOMACY_UNIT_ID === id;
	});
}

function getAdvancedOrdersForId(id, orders) {
	return orders.filter(function(order) {
		return order.@GameScreenController.DIPLOMACY_UNIT_ID === id;
	});
}

function removeHeading(id) {
	$(".heading." + id).remove();
}

function createBasicOrder(id, orders, targetLocationSelector, emptyTargetMessage) {
	var basicOrdersForId = getBasicOrdersForId(id, orders);
	var basicHeading = getBasicHeading(id);
	
	var optionElements = transformBasicOrdersIntoElements(basicOrdersForId);
	var selectElement = document.createElement("select");
	document
	selectElement.setAttribute("name", 
			"@MovementPhaseOrderHandler.SOURCE_PREFIX" + id);
	selectElement.className = "@GameScreenController.FIRST_TARGET_SELECT";
	optionElements.forEach(function(optionElement) {
		selectElement.appendChild(optionElement);
	});
	
	
	$("#" + id).before(basicHeading);
	if (optionElements.length > 0) {
		$(targetLocationSelector).append(selectElement);
	} else {
		$(targetLocationSelector).append(emptyTargetMessage);
	}
	

	prepareBasicLine(id);
}

function prepareBasicLine(id) {
	var sourceName =
		$("#" + id + " .@GameScreenController.PRESENTATION_NAME").text();
	var basicTargetName =
		$("#" + id + " .@GameScreenController.FIRST_TARGET_SELECT").
		map(function (idx, selectElement) {
			return selectElement.options[selectElement.selectedIndex];
		}).text();
	
	var line = 
		getLine(sourceName, basicTargetName, 5, "defaultorder unitfrance");
	
	return 0;
}

function createAdvancedOrder(id, orders, 
		sourceLocationSelector,
		targetLocationSelector, 
		emptySourceMessage, emptyTargetMessage) {
	var advancedOrdersForId = getAdvancedOrdersForId(id, orders);
	var advancedHeading = getAdvancedHeading(id);
	
	var uniqueSourceOrders = getUniqueSourceOrders(advancedOrdersForId);
	var sourceOptionElements = 
		getSourceOptionElements(id, 
			uniqueSourceOrders, advancedOrdersForId);
	
	var selectElement = document.createElement("select");
	selectElement.className = "@GameScreenController.SOURCE_UNIT";
	selectElement.setAttribute("name", 
			"@MovementPhaseOrderHandler.SOURCE_PREFIX" + id)
	sourceOptionElements.forEach(function(optionElement) {
		selectElement.appendChild(optionElement);
	});
	
	$(selectElement).change(function() {
		var value = this.value;
		$(sourceLocationSelector).empty();
		
		var targets = advancedOrdersForId.filter(function(elem) {
			return elem.@GameScreenController.SOURCE_LOCATION_ID === value;
		});
		
		var targetOptionElements = getTargetOptionElements(targets);
		var selectElement = document.createElement("select");
		selectElement.className = "@GameScreenController.TARGET_UNIT";
		selectElement.setAttribute("name", 
				"@MovementPhaseOrderHandler.TARGET_PREFIX" + id)
		targetOptionElements.forEach(function(elem) {
			selectElement.appendChild(elem);
		});
		
		$(sourceLocationSelector).append(selectElement);
	});
	
	$("#" + id).before(getAdvancedHeading(id));
	if (sourceOptionElements.length > 0) {
		$(targetLocationSelector).append(selectElement);
	} else {
		$(targetLocationSelector).append(emptySourceMessage);
	}
	
}

function getUniqueSourceOrders(advancedOrders) {
	return advancedOrders.reduce(function(list, order) {
		var doesExist = list.some(function(uniqueOrder) {
			return order.@GameScreenController.SOURCE_LOCATION_ID === 
				uniqueOrder.@GameScreenController.SOURCE_LOCATION_ID;
		});
		
		if (!doesExist) {
			list.push(order);
		}
		
		return list;
	}, []);
}

function getSourceOptionElements(id, advancedOrdersForSource, allOrdersForId) {
	return advancedOrdersForSource.map(function(order) {
		var optionElement = document.createElement("option");
		optionElement.textContent = 
			order.@GameScreenController.SOURCE_PRESENTATION_NAME;
		optionElement.setAttribute("value", order.@GameScreenController.SOURCE_LOCATION_ID);
		
		return optionElement;
	});
}

function getTargetOptionElements(targetList) {
	return targetList.map(function(target) {
		var optionElement = document.createElement("option");
		optionElement.textContent = target.@GameScreenController.TARGET_PRESENTATION_NAME;
		optionElement.setAttribute("value", target.@GameScreenController.TARGET_LOCATION_ID);
	
		return optionElement;
	})
}

function transformBasicOrdersIntoElements(orders) {
	return orders.map(function(order) {
		var option = document.createElement("option");
		option.textContent = order.@GameScreenController.PRESENTATION_NAME;
		option.setAttribute("value", order.@GameScreenController.LOCATION_ID);
	
		return option;
	});
}

$(document).ready(function() {
	$(".@GameScreenController.UNIT_ORDER").change(function() {
		var tableRow = this.parentElement.parentElement;
		var id = this.parentElement.parentElement.id;
		var idSelector = "#" + id;
		var targetLocationSelector = idSelector + " .@GameScreenController.TARGET_LOCATION";
		var sourceLocationSelector = idSelector + " .@GameScreenController.SOURCE_LOCATION";
		
		$(targetLocationSelector).empty();
		$(sourceLocationSelector).empty();
		removeHeading(id);
		
		$(".orderDrawing-" +id).remove();
		
		switch($(this).val()) {
			case "@OrderType.HOLD": {
				break;
			}
			case "@OrderType.MOVE": {
				createBasicOrder(id, getPotentialMoveOrders(), targetLocationSelector);
				break;
			}
			case "@OrderType.SUPPORT_HOLD": {
				createBasicOrder(id, 
					getPotentialSupportHoldOrders(), 
					targetLocationSelector,
					"No support holds for this unit");
				break;
			}
			case "@OrderType.SUPPORT_MOVE": {
				createAdvancedOrder(id, getPotentialSupportMoveOrders(),
					sourceLocationSelector, targetLocationSelector,
					"No support moves for this unit",
					"No suppr"
				)
				break;
			}
			case "@OrderType.CONVOY": {
				createAdvancedOrder(id, getPotentialConvoyOrders(),
					sourceLocationSelector, targetLocationSelector,
					"No convoys for this unit",
					"No support moves for this unit");
				break;
			}
		}
		
		
	});
	
	
	
});