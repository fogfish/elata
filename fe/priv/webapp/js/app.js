/*
%%
%%   Copyright (c) 2011, Nokia Corporation
%%   All Rights Reserved.
%%
%%   The contents of this file are subject to the 3-clause BSD License,
%%   (the "License"); you may not use this file except in compliance 
%%   with the License. You should have received a copy of the 3-clause
%%   BSD Licensee along with this software. If not, it can be
%%   retrieved online at http://www.opensource.org/licenses/BSD-3-Clause.
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%   the License for the specific language governing rights and limitations
%%   under the License.
%%
*/

// namespaces
var elata = {
	view : {}
};

// application
elata.App = new Class({
	view        : null,   // active view
	username    : null,
	usermail    : null,
	
	locationId  : null,
	serviceId   : null,
	usecaseId   : null,
   
	location    : null,	// list of locations
	user        : null,   // user object
	usecase     : null,   // usecase object
   	
	navigator   : null,	
   
	reqAttempt  : 0,
	reloadTimer	: null, //auto reload timer
	reloadDelay : 300000, //auto reload delay
	
	initialize: function()
	{
		//set global click handler for view management
		window.addEvent('click', 
			function(e)
			{
				var link = (e.target.localName == 'a') ? e.target : e.target.getParent('a');
				if (link && link.hash != null && link.hash != '')
				{
					//parse hash
					var list = link.hash.substring(1).split('-');
					this.serviceId = list[1];
					this.usecaseId = list[2];
					switch (list[0]) {
					case 'usecase':
						var view = elata.view.UseCase;
						break;
						
					default:
						var view = elata.view.Main;
						break;
					}
					//update window location
					this.updateLocationHash(link.hash);
					e.preventDefault();
				}	
			}.bind(this)
		);
		window.addEvent('keydown', function(e) { this.view.keydown(this, e) }.bind(this) );
		this_ = this;
		function locationHashChanged() {
			if (window.location.hash === "#main" || window.location.hash === "#signin") { //show or hide back btn
				$('naviBack').setStyle('display', 'none');
			} else {
				$('naviBack').setStyle('display', 'inline-block');
			}
			if (window.location.hash !== this.lastChangedHash) {
				this.lastChangedHash = window.location.hash;
				this_.setView(this_.getViewByHash(window.location.hash));
			}
		}
		window.onhashchange = locationHashChanged;
		this.navigator = new elata.Navigator();
		this.navigator.addEvent('change',  this.activateView.bind(this));
		this.grid      = new elata.Grid('image-grid', 'navi-vbar', 'navi-hbar');
		this.updateLocationHash('#signin');
	},
	
	setView: function(view)
	{
		if (this.view) {
			this.view.deactivate(this);
		}
		if (this.reloadTimer) { //clear timer from previous view
			window.clearTimeout(this.reloadTimer);
			this.reloadTimer = null;
		}
		this.view = view;
		this.view.activate(this);
	},
	
	// create request object
	newRequest: function(uri)
	{
		var req = new Request({url: uri, emulation: false, urlEncoded: false});
	   req.setHeader('Accept', 'application/erlang-base64');
	   req.onSuccess = this.showContent.bind(this, [uri, req]);
	   req.onFailure = this.errorContent.bind(this, [uri, req]);
	   return req;
	},
	
	reqContent: function()
	{
		this.view.reqContent(this);
	},
   
	showContent: function(uri, req)
	{
		//set auto reload timer if not present
		if (this.reloadTimer === null) {
			this.reloadTimer = window.setTimeout(
				function(app) {
					app.reloadTimer = null; //clear the id
					app.view.reqContent(app);
				}, 
				this.reloadDelay,
				this //pass the app into the function
			);
		}
		this.view.showContent(this, uri, req.response.text);
	},
   
	errorContent: function(uri, req)
	{
		if (req)
		   this.view.errorContent(this, uri, req.status);
		else
			this.view.errorContent(this, uri, 500);
	},
   
	activateView: function()
	{
		this.view.activateView(this, this.navigator.getSelected());
	},
	
	updateLocationHash: function(hash)
	{
		window.location.hash = hash; //update location with new hash
	},
	
	getViewByHash: function(hash)
	{
		var list = hash.substring(1).split('-');
		switch (list[0]) {
		case 'usecase':
			var view = elata.view.UseCase;
			break;

		case 'signin':
			var view = elata.view.Signin;
			break;
			
		case 'addusecase':
			var view = elata.view.NewUseCase;
			break;

		default:
			var view = elata.view.Main;
			break;
		}
		return view;
	}
});