function NotOptionException() {}

function Option() {
	this.map = function(mappingFunction) {
		if (this instanceof Some) {
			return new Some(mappingFunction(this.value));
		} else {
			return new None();
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
