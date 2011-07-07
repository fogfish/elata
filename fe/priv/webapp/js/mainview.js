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
elata.view.Main = {
	
	activate: function(app)
	{
		app.reqContent();
		$('user').setStyle('display', 'block');
	},
	
	deactivate: function(app)
	{
		$('user').setStyle('display', 'none');
	},
	
	reqContent: function(app)
	{
		app.reqAttempt = 0;
		app.newRequest('/config/location').get();
	},
	
	showContent: function(app, uri, data)
	{
	    // parse list of locations
	    var tuple = Bert.decode(
		    Bert.bytes_to_string(Base64.decode(data))
	    );
	    app.location = elata.kvset.decode(tuple);
	    // sort location by sys_id
	    app.location.sort(
    		function(a, b)
    		{
    			return ((a.sys_id < b.sys_id) ? -1 : ((a.sys_id > b.sys_id) ? 1 : 0)); 
    		}
	    );
	    //console.log( 'selected', app.navigator.getSelected() );
	    if ( app.navigator.getSelected().length ) {
	    	app.view.renderView(app);
	    } else {
	    	app.view.renderNavi(app);
	    }
	},
	
	errorContent: function(app, uri, err)
	{
		app.reqAttempt++;
		if (app.reqAttempt < 5)
		{
			app.newRequest(uri).get();
		} else {
			//TODO: error dialog
			alert('Fatal: Server Error.');
		}
	},
	
	activateView: function(app, view)
	{
		app.locationId = view[0];
      
		if (app.user)
			elata.view.Main.renderView(app);
	},
	
	keydown: function(app, e)
	{
	},
	
	renderView: function(app)
	{
		var ui = $('user').getElement('.list').empty();
		var list = (new Element('dl')).inject(ui); 
		
		for(var sid in app.user.service)
		{
			var s = app.user.service[sid];
			var title = (new Element('dt', {html: s.title})).inject(list);
			var items = (new Element('dd')).inject(list);
			var ilist = new Element('ul').inject(items);
	   	
		   	for(var cid in s.usecase)
		   	{
		   		var u = s.usecase[cid];
		   		var tele = (u.telemetry && u.telemetry[app.locationId]) ? (u.telemetry[app.locationId] / 1000000).toFixed(3) : 'N/A';
		   		
		   		app.view.newUseCase(
	   				"usecase-" + sid + "-" + cid,
		   			u.title, 
		   			u.uri, 
		   			tele
		   		).inject(ilist);
		   	}	
		}
	},
	
	newUseCase: function(link, title, uri, latency)
	{
		var item   = new Element('li');
		var link   = (new Element('a',   {href: '#' + link})).inject(item);
		(new Element('div', {'class': 'label title',   html: title})).inject(link);
		(new Element('div', {'class': 'label uri',     html: uri})).inject(link);
		(new Element('div', {'class': 'label latency', html: latency + ' sec'})).inject(link);
		return item;
	},
	
	renderNavi: function(app)
	{
		app.navigator.setItems(0, 'at', app.location);
		app.navigator.select([ app.location[0].sys_id ]); //TODO: location frm user profile
	}
}