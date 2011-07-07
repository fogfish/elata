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
elata.view.Signin = {
	 
	/**
	 * Activate view Signin
	 * @param {object} app Application object
	 */
	activate: function(app)
	{
	    app.username = Cookie.read('xSESSION'); 
		if (app.username)
		{
			app.usermail = 'xSESSION';
			app.reqContent();
		} else {
			// show UI
			$('signin').setStyle('display', 'block');
		}
		$('signin').getElement('form').addEvent('submit', function(e) {
		   app.username = $('username').value;
		   app.usermail = $('usermail').value;
		   app.reqContent();
		   e.preventDefault();	
		});	
	},
	
	deactivate: function(app)
	{
		$('signin').setStyle('display', 'none');
	},
	
	reqContent: function(app)
	{
		if (app.username && app.usermail)
		{
		   app.reqAttempt = 0;
		   app.newRequest('/user/' + app.username).get();
		}
		if (!app.username)
			$('username').getParent().addClass('required');
		else
			$('username').getParent().removeClass('required');
		
		if (!app.usermail)
			$('usermail').getParent().addClass('required');
		else
			$('usermail').getParent().removeClass('required');
			
	},
	
	showContent: function(app, uri, data)
	{
	   //user request is completed
	   if (uri == '/user/' + app.username)
	   {
		   var tuple =  Bert.decode(
			   Bert.bytes_to_string(Base64.decode(data))
		   );
		   app.user  = elata.kvset.decode(tuple);
		   app.updateLocationHash('#main');
		   //persist user id
		   Cookie.write('xSESSION', app.username, {duration: 30});
	   }
	},
	
	errorContent: function(app, uri, err)
	{
		app.reqAttempt++;
		if (app.reqAttempt < 5)
		{
			app.newRequest(uri).get();
		} else if ((app.reqAttempt >= 5) && (app.reqAttempt < 10)) {
			if (app.usermail == 'xSESSION')
			{
				// re-store session but not user record show UI
				$('signin').setStyle('display', 'block');
			} else {
				req = app.newRequest(uri);
			   req.setHeader('Content-Type', 'application/erlang-base64');
			   req.put(elata.view.Signin.defaultUserProfile(app));
			}
		} else {
			//TODO: error dialog
			alert('Fatal: Server Error.');
		}
	},
	
	activateView: function(app, view)
	{
	},
	
	keydown: function(app, e)
	{
	},
	
	
	defaultUserProfile: function(app)
	{
		var user = {
			sys_id  : app.username,
			email   : app.usermail,
			service : 
			{
				store: 
				{
					title   : 'R&D Test Service',
				   usecase : 
				   {
				   	desk :
				   	{
				   		title : 'Test URI #1',
				   		uri   : 'http://localhost:80/'
				   	},
				   	mobi :
				   	{
				   		title : 'Test URI #2',
				   		uri   : 'http://localhost:80/'
				   	}
				   }
				}
			}
		};
		var tuple = elata.kvset.encode(user);
		return Base64.encode(
			Bert.encode(tuple)
		);
	}
};
