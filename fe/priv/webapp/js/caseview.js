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
elata.view.UseCase = {
	
	activate: function(app)
	{
		app.reqContent();
		elata.view.UseCase.renderNavi(app);
		$('usecase').setStyle('display', 'block');
	},
	
	deactivate: function(app)
	{
		$('usecase').setStyle('display', 'none');
	},
	
	reqContent: function(app)
	{
		app.reqAttempt = 0;
		app.newRequest(
			'/user/' + app.username +
			'/service/' + app.serviceId +
			'/usecase/' + app.usecaseId
	   ).get();
	},
	
	showContent: function(app, uri, data)
	{
		var tuple =  Bert.decode(
			Bert.bytes_to_string(Base64.decode(data))
		);
		app.usecase  = elata.kvset.decode(tuple);
		// render view
		app.grid.reset();
		
		var views = app.usecase[app.locationId].view;
		var viewName = Object.keys(views);
		viewName.sort(function(a, b){ return ((a < b) ? -1 : ((a > b) ? 1 : 0)); });
		viewName.each(
			function(key)
			{
				var view = views[key];
				if (instanceOf(view, Array))
				{	
					app.grid.addRow();
					view.sort(function(a, b){ return ((a < b) ? -1 : ((a > b) ? 1 : 0)); });
					view.each(
						function(scale)
						{
							var item = (new Element('div', {'class' : 'item'})).inject(app.grid.c);
							var img  = (new Element('img')).inject(item);
							img.src = '/user/' + app.username + '/service/' + app.serviceId +
	   				       		'/usecase/' + app.usecaseId + '/location/' + app.locationId +
	   				       		'/image/' + key + '/scale/' + scale;
	   			
							app.grid.addItem(item);	       
						}
					);
				}
			}
		);
		app.grid.showItem(app.grid.selectedRow,app.grid.selectedCol);
		elata.view.UseCase.renderTitle(app);
	},
	
	errorContent: function(app, uri, err)
	{
		app.reqAttempt++;
		if (app.reqAttempt < 5)
		{
	   	app.newRequest(uri).get();
	   } else {
	   	$('usecase').getElement('h1').innerHTML = 'Use-case telemetry is not available yet.'
	   }
	},
	
	activateView: function(app, view)
	{
		var nLocation = view.shift();
		var nService  = view.shift();
		var nUsecase  = view.shift();
		
		// do nothing if same thing is selected
		if ( (app.locationId == nLocation) && 
			  (app.serviceId == nService)   && 
		     (app.usecaseId == nUsecase) )
		   return;
		
		if (nLocation && nService && nUsecase)
		{
			// another usecase is requested
			app.locationId = nLocation;
			app.serviceId  = nService;
			app.usecaseId  = nUsecase;
			app.setView(elata.view.UseCase);
			return;
		}
		
		if (!nService && !nUsecase)
		{
			//only location is changed
			app.locationId = nLocation;
			app.setView(elata.view.UseCase);
			return;
		}
		
		if (!nUsecase)
		{
			//service is changed
			app.locationId = nLocation;
			app.serviceId  = nService;
			// use-case is unknow let's pick a first use-case at services
			var s = app.user.service[nService];
			app.usecaseId  = Object.keys(s.usecase)[0];
			app.setView(elata.view.UseCase);
			return;
		}
	},
	
	renderNavi: function(app)
	{
	   app.navigator.setItems(0, 'at', app.location);
	   // of service
	   var service = [];
	   for (var key in app.user.service)
	   {
	   	service.push({
	   		sys_id: key,
	   		title : app.user.service[key].title
	   	});
	   };
	   service.sort(function(a, b){ return ((a.sys_id < b.sys_id) ? -1 : ((a.sys_id > b.sys_id) ? 1 : 0)); });
	   app.navigator.setItems(1, 'of', service);
	   
	   // for use-case
	   var usecase = [];
	   for (var key in app.user.service[app.serviceId].usecase)
	   {
	   	usecase.push({
	   		sys_id: key,
	   		title : app.user.service[app.serviceId].usecase[key].title
	   	});
	   }
	   usecase.sort(function(a, b){ return ((a.sys_id < b.sys_id) ? -1 : ((a.sys_id > b.sys_id) ? 1 : 0)); });
	   app.navigator.setItems(2, 'for', usecase);
	   
	   app.navigator.select([ app.locationId, app.serviceId, app.usecaseId ]); //TODO: location frm user profile
	},
	
	renderTitle: function(app)
	{
		var views = app.usecase[app.locationId].view;
	   var viewName = Object.keys(views);
	   viewName.sort(function(a, b){ return ((a < b) ? -1 : ((a > b) ? 1 : 0)); });
      var viewId   = viewName[app.grid.row]; 
		
		var scales = app.usecase[app.locationId].view[viewId];
		var scale  = scales[app.grid.col];
		
		var m = parseInt( scale / (3600 * 24 * 30))
		var w = parseInt( scale / (3600 * 24 * 7));
		var d = parseInt( scale / (3600 * 24)) % 30;
		var h = parseInt( scale / 3600 ) % 24;
		
		var title = elata.locale.en[viewId] + ' / ';
		if (m > 1)
			title += m + elata.locale.en['monthes'];
		else if (m > 0)
			title += m + elata.locale.en['month'];
		else if (w > 1)
			title += w + elata.locale.en['weeks'];
		else if (w > 0)
			title += w + elata.locale.en['week'];
		else if (d > 1) 
			title += d + elata.locale.en['days'];
		else if (d > 0)
		   title += d + elata.locale.en['day'];
		else if (h > 1)
			title += h + elata.locale.en['hours'];
		else if (h > 0)
			title += h + elata.locale.en['hour'];
		
		$('usecase').getElement('h1').innerHTML =  title + '<div>' + app.usecase.sys_id + '</div>'; 
		
	},
	
	
	keydown: function(app, e)
	{
		//Note: this is bound to app instance
		switch(e.key)
		{
		case 'left':
			app.grid.prevCol();
			elata.view.UseCase.renderTitle(app);
			e.preventDefault();
			break;
		case 'right':
			app.grid.nextCol();
			elata.view.UseCase.renderTitle(app);
			e.preventDefault();
			break;
		case 'up':
			app.grid.prevRow();
			elata.view.UseCase.renderTitle(app);
			e.preventDefault();
			break;
		case 'down':
			app.grid.nextRow();
			elata.view.UseCase.renderTitle(app);
			e.preventDefault();
			break;
		}
		// update vbar
		//$('vbar').innerHTML = '&#x25c6;<br/>&#x25c7;<br/>&#x25c7;'
	}
}