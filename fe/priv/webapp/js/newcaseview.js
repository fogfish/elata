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
elata.view.NewUseCase = {
	
	activate: function(app)
	{
		$('editor').setStyle('display', 'block');
		$('editor').getElement('form').addEvent('submit', function(e){
	      app.reqContent();
			e.preventDefault();	
		});
	},
	
	deactivate: function(app)
	{
		$('editor').setStyle('display', 'none');
	},
	
	reqContent: function(app)
	{
		//
		// validate user input
		var valid = true;
		if ($('fservice').value == '')
		{
			$('fservice').getParent().addClass('required');
			valid = false;
		} else {
		   $('fservice').getParent().removeClass('required');
		}
		
		if ($('fusecase').value == '')
		{
			$('fusecase').getParent().addClass('required');
			valid = false;
		} else {
		   $('fusecase').getParent().removeClass('required');
		}
		
		if (!elata.view.NewUseCase.is_uri($('furi').value))
		{
			$('furi').getParent().addClass('required');
			valid = false;
		} else {
		   $('furi').getParent().removeClass('required');
		}
		
		if ($('fua').value == '')
		{
			$('fua').getParent().addClass('required');
			valid = false;
		} else {
		   $('fua').getParent().removeClass('required');
		}
		
		if (valid)
		{
		   uri = '/user/' + app.username + 
		         '/service/' + fnv1a($('fservice').value) +
		         '/usecase/' + fnv1a($('fusecase').value);
		   data = {
			sys_id : 'none',
			   service: $('fservice').value,
		      usecase: $('fusecase').value,
		      uri    : $('furi').value,
		      ua     : $('fua').value
		   };
		
		   req = app.newRequest(uri);
	      req.setHeader('Content-Type', 'application/erlang-base64');
	      req.put( 
	         Base64.encode(
	      	   Bert.encode(elata.kvset.encode(data))
	         ) 
	      );      
		} 
	   
	},
	
	showContent: function(app, uri, data)
	{
		app.setView(elata.view.Main);
	},
	
	errorContent: function(app, uri, err)
	{
	},
	
	activateView: function(app, view)
	{
	},
	
	keydown: function(app, e)
	{
	},
	
	is_uri: function(uri)
	{
	   var regexp = /(http|https):\/\/(\S+)(:[0-9]+)(\/|\/([\w#!:.?+=&%@!\-\/]))/
      return regexp.test(uri);
	}
}


function fnv1a(text)
{
     var hash = 2166136261;
     for (var i = 0; i < text.length; ++i)
     {
          hash ^= text.charCodeAt(i);
          hash += (hash << 1) + (hash << 4) + (hash << 7) +
            (hash << 8) + (hash << 24);
     }
    return hash >>> 0;
}