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

/*
%%
%% Application
%%
*/
var user    = null;
var key     = null;
var ui_nav  = null;
var ui_dsh  = null;
var stack   = [];

String.prototype.escapeHTML = function () {                                        
        return(                                                                 
            this.replace(/&/g,'&amp;').                                         
                replace(/>/g,'&gt;').                                           
                replace(/</g,'&lt;').                                           
                replace(/"/g,'&quot;')                                         
        );                                                                      
    };
    
function main()
{
	ui_nav = new Navigator([$('nav-agent')]);
	ui_dsh = $('dashboard').getElement('.main').getElement('ul');
	
	/* register user */
   $('action-signin').addEvent('click', function(e) {
      e.preventDefault();	
		new_user_profile();
	});
	/* add new use-case */
	$('action-add').addEvent('click', function(e) {
		e.preventDefault();
		ui_usecase();
	});
	/* remove use-case */
	$('action-del').addEvent('click', function(e) {
	   e.preventDefault();
	   remove_usecase();
	});
	/* create new use-case */
	$('action-create').addEvent('click', function(e) {
		e.preventDefault();	
		new_usecase();
	});
	/* create new use-case */
	$('action-update').addEvent('click', function(e) {
		e.preventDefault();	
		update_usecase();
	});
	/* edit use-case */
	$('action-edit').addEvent('click', function(e) {
	   e.preventDefault();
	   ui_edit_usecase();
	});
	$('action-out').addEvent('click', function(e) {
		e.preventDefault();
		Cookie.dispose('xUSERNAME');
		view_show('signin');
		stack = [];
	});
	
	/* listen changes at navigator */
	ui_nav.addEvent('change', function(e) {
	   ui_usecases_update()
	});
	/* switch to statistic view */
	$('action-stat').addEvent('click', function(e){
	   e.preventDefault();
	   ui_statistic();
	});
	/* switch scale */
	$('tscale').addEvent('change', function(e){
			ui_statistic();
	});
	
	$('stat').getElement('a.back').addEvent('click', function(e){
		view_back();	
	});
	$('viewer').getElement('a.back').addEvent('click', function(e){
		view_back();	
	});
	$('editor').getElement('a.back').addEvent('click', function(e){
		view_back();	
	});
	
	get_location();
	get_user_profile();
}


/*
%%
%% Get list of user profile
%%
*/
function get_user_profile()
{
   var username = Cookie.read('xUSERNAME');	
   if (username)
   	new Request.JSON({
   	   url: '/' + username,
   	   timeout: 20,
   	   onTimeout: ui_user_signin,
   	   onFailure: ui_user_signin,
   	   onSuccess: ui_user_profile
   	}).get();
   else	
   	ui_user_signin();
}

function new_user_profile()
{
   if (!$('username').value)
	   $('username').getParent().addClass('required');
   else
	   $('username').getParent().removeClass('required');
			
	if (!$('usermail').value)
	   $('usermail').getParent().addClass('required');
	else
	   $('usermail').getParent().removeClass('required');
		   
	if ($('username').value && $('usermail').value)
	   new Request.JSON({
			url: '/user',
			onSuccess: ui_user_profile,
			onFailure: ui_user_signin
		}).post(JSON.encode({username: $('username').value, usermail: $('usermail').value}));
}

function remove_usecase()
{
	new Request.JSON({
		emulation: false,	
	   url: '/' + user.username + '/proc/' + key,
	   onSuccess: get_user_profile
	}).delete();
}

function ui_user_signin()
{
	view_show('signin');
}


function ui_user_profile(obj)
{
	//console.log(obj);
	user = obj;
	Cookie.write('xUSERNAME', user.username, {duration: 30});
	
	view_show('dashboard');
	ui_dsh.empty();
	
	for(id in user.usecase)
	{
		var pid = uri(id);
      ui_usecase_summary(id, user.usecase[id]).inject(ui_dsh);
	   new Request.JSON({
	      url: '/' + user.username + '/' + pid.schema + pid.path,
	      onSuccess: ui_usecase_details
	   }).get();		
	};
}



function ui_usecase()
{
	view_show('editor');
	$('action-del').getParent().setStyle('display', 'none');
	$('action-update').getParent().setStyle('display', 'none');
	$('action-create').getParent().setStyle('display', 'block');
}

function ui_edit_usecase()
{
	view_show('editor');
	$('action-del').getParent().setStyle('display', 'block');
	$('action-create').getParent().setStyle('display', 'none');
	$('action-update').getParent().setStyle('display', 'block');
	
	var u = user.usecase[key];
	var title = u.title.split("/", 2);
	
	$('fservice').value = title[0];
	$('fusecase').value = title[1];
	$('furi').value = u.script;
	$('fua').value = u.http[0].substring(11);
	$('fthinktime').value = u.thinktime;
}


function ui_usecase_summary(id, uc)
{
	var locId = ui_nav.getSelected()[0];
	title = uc.title.split("/", 2);
   var li = (new Element('li', {id: id}));
	var a  = (new Element('a')).inject(li);
	(new Element('small', {html: 'PENDING'})).inject(a);
	var icn = (new Element('img')).inject(a);
   icn.src = '/style/pending.png';
	
   var d  = (new Element('div')).inject(a);
	(new Element('span', {html: title[0]})).inject(d);	
   (new Element('big',  {html: title[1]})).inject(d);	
   (new Element('span', {html: "n/a"})).inject(d);	
   li.addEvent('click', function(e) {
   	key = id;	
      if (e.target == icn)
         ui_statistic();
      else
      	spec_usecase(id)
   });
   return li;
}


function ui_usecase_details(uc)
{
	// merge case details to existed value
	var ucase = user.usecase[uc.id];
	for (key in uc)
		ucase[key] = uc[key];
	//console.log(user);
	ui_usecases_update();
}

function ui_usecases_update()
{
	var locId = ui_nav.getSelected()[0];
	for(id in user.usecase)
	{
      var ui = $(id);
      var u  = user.usecase[id];
      ui.getElements('span')[1].innerHTML = u.script;
      ui.getElement('img').src = u.telemetry[locId].icon;
	   if (u.telemetry && u.telemetry[locId])
	      ui.getElement('small').innerHTML = (u.telemetry[locId].latency / 1000).toFixed(3) + ' ms';
      else
	      ui.getElement('small').innerHTML = 'PENDING';
	};
}

function spec_usecase(id)
{
	new Request.JSON({
	      url: '/' + user.username + '/telemetry/' + id,
	      onSuccess: function(tele){ ui_usecase_spec(id, tele) }
	}).get();
}

function ui_usecase_spec(id, tele)
{
	key = id;
	/* merge telemetry */
	var u = user.usecase[id];
	u.telemetry = tele;
	//console.log(u);
	
	view_show('viewer');
	
	$('viewer').getElement('.location').innerHTML = ui_nav.getSelected()[0];
	
	var title = u.title.split("/", 2);
	$('li-service').getElement('span').innerHTML = title[0];
	$('li-usecase').getElement('span').innerHTML = title[1];
	$('li-script').getElement('span').innerHTML = u.script;
	$('li-http').getElement('p').innerHTML = u.http;
	$('li-thinktime').getElement('span').innerHTML = u.thinktime;
	
	var locId = ui_nav.getSelected()[0];
	if (tele[locId].latency != 0)
	   $('li-latency').getElement('span').innerHTML = (tele[locId].latency / 1000).toFixed(3) + ' ms';
	else
		$('li-latency').getElement('span').innerHTML = 'PENDING'
	
	if (tele[locId].dns != 0)
	   $('li-dns').setStyle('display', 'block').getElement('span').innerHTML = (tele[locId].dns / 1000).toFixed(3) + ' ms';
	else
		$('li-dns').setStyle('display', 'none');
		
	if (tele[locId].tcp != 0)
	   $('li-tcp').setStyle('display', 'block').getElement('span').innerHTML = (tele[locId].tcp / 1000).toFixed(3) + ' ms';
	else
		$('li-tcp').setStyle('display', 'none');
	
	if (tele[locId].ssl != 0)
	   $('li-ssl').setStyle('display', 'block').getElement('span').innerHTML = (tele[locId].ssl / 1000).toFixed(3) + ' ms';
	else
		$('li-ssl').setStyle('display', 'none');
	
	if (tele[locId].ttfb != 0)
	   $('li-ttfb').setStyle('display', 'block').getElement('span').innerHTML = (tele[locId].ttfb / 1000).toFixed(3) + ' ms';
	else
		$('li-ttfb').setStyle('display', 'none');
	
	if (tele[locId].ttmr != 0)
	   $('li-ttmr').setStyle('display', 'block').getElement('span').innerHTML = (tele[locId].ttmr / 1000).toFixed(3) + ' ms';
	else
		$('li-ttmr').setStyle('display', 'none');
	
	if (tele[locId].recv_oct != 0)
	   $('li-size').setStyle('display', 'block').getElement('span').innerHTML = (tele[locId].recv_oct / 1024).toFixed(3) + ' KB';
	else
		$('li-size').setStyle('display', 'none');
	
	if (tele[locId].recv_cnt != 0)
	   $('li-pckt').setStyle('display', 'block').getElement('span').innerHTML = tele[locId].recv_cnt;
	else
		$('li-pckt').setStyle('display', 'none');
	
	if (tele[locId].recv_avg != 0)
	   $('li-pckt-size').setStyle('display', 'block').getElement('span').innerHTML = tele[locId].recv_avg + ' B';
	else
		$('li-pckt-size').setStyle('display', 'none');
	
	$('li-doc').getElement('pre').innerHTML = tele[locId].doc.escapeHTML();
}


function new_usecase()
{
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
		
	if ($('furi').value == '')
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
         
	if ($('fthinktime').value == '')
	{
		$('fthinktime').getParent().addClass('required');
		valid = false;
	} else {
		$('fthinktime').getParent().removeClass('required');
	}

	username = Cookie.read('xUSERNAME');
	if (valid && username)
	{
		new Request.JSON({
			url: '/' + username + '/process',
			onSuccess: get_user_profile
		}).send({
		   method: 'post',
		   data: JSON.encode({
		      usecase   : $('fservice').value + "/" + $('fusecase').value,
		      script    : $('furi').value,
		      thinktime : parseInt($('fthinktime').value),
		      http      : [ "User-Agent:" + $('fua').value ]
		   })
      });		
   }	
}


function update_usecase()
{
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
		
	if ($('furi').value == '')
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
         
	if ($('fthinktime').value == '')
	{
		$('fthinktime').getParent().addClass('required');
		valid = false;
	} else {
		$('fthinktime').getParent().removeClass('required');
	}

	username = Cookie.read('xUSERNAME');
	if (valid && username)
	{
		new Request.JSON({
		   emulation: false,	
	      url: '/' + user.username + '/proc/' + key,
	      onSuccess: function() {
	      	new Request.JSON({
			      url: '/' + username + '/proc',
			      onSuccess: get_user_profile
		      }).send({
		         method: 'post',
		         data: JSON.encode({
		            usecase   : $('fservice').value + "/" + $('fusecase').value,
		            script    : $('furi').value,
		            thinktime : parseInt($('fthinktime').value),
		            http      : [ "User-Agent:" + $('fua').value ]
		         })
            });
	      }
	   }).delete();
	}
}


function get_location()
{
	new Request.JSON({
	   url: '/config/agent',
	   onSuccess: ui_location,
	   onFailure: get_location
	}).get();
}

function ui_location(list)
{
   //console.log(list);
	ui_nav.setItems(0, list);
}


function ui_statistic()
{
	view_show('stat');
	$('stat').getElement('.location').innerHTML = ui_nav.getSelected()[0];
	/* scale */
	var sel = $('tscale');
	var opt = sel.options[ sel.selectedIndex ];
	var scale = opt.getAttribute('value');
	/* loc id */
	var locId = ui_nav.getSelected()[0];
	var ui = $('stat').getElement('.viewport').empty();
   var i0 = (new Element('img')).inject(ui);
   i0.src = '/view/' + locId + '/' + key + '/uri.image/' + scale; 
   
   var i1 = (new Element('img')).inject(ui);
   i1.src = '/view/' + locId + '/' + key + '/latency.image/' + scale;
   
   var i5 = (new Element('img')).inject(ui);
   i5.src = '/view/' + locId + '/' + key + '/dns.image/' + scale;
   
   
   var i2 = (new Element('img')).inject(ui);
   i2.src = '/view/' + locId + '/' + key + '/tcp.image/' + scale;
   
   var i3 = (new Element('img')).inject(ui);
   i3.src = '/view/' + locId + '/' + key + '/http.image/' + scale;
   
   var i4 = (new Element('img')).inject(ui);
   i4.src = '/view/' + locId + '/' + key + '/availability.image/' + scale;
}



function view_show(id)
{
   $$('.view').each(function(v){
   	if (v.getProperty('focused') == 'true')
   		stack.push(v.getProperty('id'));
      v.setProperty('focused', 'false')
   });
	$(id).setProperty('focused', 'true');	
	
}

function view_back()
{
	var id = stack.pop();
	$$('.view').each(function(v){
      v.setProperty('focused', 'false')
   });
	$(id).setProperty('focused', 'true');
}


//-------------------------------------------------------------------
//
// URI parser
//
//-------------------------------------------------------------------
function uri(Str)
{
	var obj   = {};
	var acc   = "";
	var state = 0;
   for (i = 0; i < Str.length; i++)
   {
      if ((state == 0) && (Str.charAt(i) == ':'))
      {	
         obj.schema = acc;
         acc = "";
      } else if (acc == "//") {
      	acc   = "";
      	state = 1;
      } else if ((state == 1) && (Str.charAt(i) == '/')) {
      	obj.authority = acc;
      	acc   = "/";
      	state = 2;
      } else {
      	acc = acc + Str.charAt(i);
      }
   };
   obj.path = acc;
   return obj;	
}




