sap.ui.define(["sap/ui/core/mvc/Controller",
	"sap/m/MessageBox",
	"sap/ui/core/routing/History"
], function(BaseController, MessageBox, History) {
	"use strict";

	return BaseController.extend("com.sap.build.sap.faultyAddresses.controller.Popover1", {
		setRouter: function(oRouter) {
			this.oRouter = oRouter;

		},
		getBindingParameters: function() {
			return {};

		},
		onInit: function() {
			this.mBindingOptions = {};
			this._oDialog = this.getView().getContent()[0];

		},
		onExit: function() {
			this._oDialog.destroy();

		}
	});
}, /* bExport= */ true);