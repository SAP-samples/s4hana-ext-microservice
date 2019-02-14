sap.ui.define([
	"com/sap/build/sap/faultyAddresses/controller/BaseController",
	"sap/ui/model/json/JSONModel",
	"sap/ui/core/routing/History"
], function(BaseController, JSONModel, History) {
	"use strict";

	return BaseController.extend("com.sap.build.sap.faultyAddresses.controller.App", {

		onInit: function() {
			var oViewModel,
				oListSelector = this.getOwnerComponent().oListSelector,
				iOriginalBusyDelay = this.getView().getBusyIndicatorDelay();

			oViewModel = new JSONModel({
				busy: true,
				delay: 0
			});
			this.setModel(oViewModel, "appView");

			// Makes sure that master view is hidden in split app
			// after a new list entry has been selected.
			oListSelector.attachListSelectionChange(function() {
				this.byId("idAppControl").hideMaster();
			}, this);

			// apply content density mode to root view
			this.getView().addStyleClass(this.getOwnerComponent().getContentDensityClass());
			return new Promise(function(fnResolve) {
				var oModel, aPromises = [];
				oModel = this.getOwnerComponent().getModel();
				aPromises.push(oModel.metadataLoaded);
				return Promise.all(aPromises).then(function() {
					oViewModel.setProperty("/busy", false);
					oViewModel.setProperty("/delay", iOriginalBusyDelay);
					fnResolve();
				});
			}.bind(this));
		}
	});
});