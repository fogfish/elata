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
var proc    = {};
var lid     = null;
var pid     = null;
var stack   = [];

/*
var key     = null;
var ui_nav  = null;
var ui_dsh  = null;
*/


    
function main()
{
	get_user_profile();
	get_cluster();
	
	$('back').addEvent('click', function(e){
		e.preventDefault();	
	   view_back();	
	});
	
	/* register user */
   $('action-signin').addEvent('click', function(e) {
      e.preventDefault();	
		put_user_profile();
	});
	
	/* add new use-case */
	$('action-add').addEvent('click', function(e) {
      e.preventDefault();
      if (view == 'editor')
		   put_usecase();
		else
		   ui_new_usecase();
	});
	$('action-update').addEvent('click', function(e) {
		e.preventDefault();	
		put_existed_usecase();
	});
	
	/* switch scale */
	$('tscale').addEvent('change', function(e){
	   ui_graph(proc);
	});
	
	/* switch to statistic view */
	$('action-stat').addEvent('click', function(e){
	   e.preventDefault();
	   ui_graph();
	});
	
	/* edit use-case */
	$('action-edit').addEvent('click', function(e) {
	   e.preventDefault();
	   ui_edit_usecase();
	});
	/* remove use-case */
	$('action-del').addEvent('click', function(e) {
	   e.preventDefault();
	   remove_usecase();
	});
	$('action-out').addEvent('click', function(e) {
		e.preventDefault();
		Cookie.dispose('xUSERNAME');
		ui_user_signin();
	});
	
	// /* create new use-case */
	// $('action-create').addEvent('click', function(e) {
		// e.preventDefault();	
		// new_usecase();
	// });
	// /* create new use-case */
	
	
	
	// 
	// /* listen changes at navigator */
	// ui_nav.addEvent('change', function(e) {
	   // ui_usecases_update()
	// });
	
	// $('stat').getElement('a.back').addEvent('click', function(e){
		// view_back();	
	// });
	// $('viewer').getElement('a.back').addEvent('click', function(e){
		// view_back();	
	// });
	// $('editor').getElement('a.back').addEvent('click', function(e){
		// view_back();	
	// });
	// 
	// get_location();
	// get_user_profile();
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
   	   onFailure: ui_user_signin,
   	   onSuccess: ui_user_profile
   	}).get();
   else	
   	ui_user_signin();
}

function put_user_profile()
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
			onFailure: function() {
   	   	alert('Unable to sigin. Invalid user credentials!'),
   	      ui_user_signin()
   	   }
		}).post(JSON.encode({
		   username: $('username').value, 
		   usermail: $('usermail').value
		}));
}


function ui_user_signin()
{
	$('signin').setStyle('display', 'block');
	$('main').setStyle('display', 'none');
	stack = [];
}


function ui_user_profile(profile)
{
	//console.log(profile);
	user = profile;
	$('username1').innerHTML = user.username;
	Cookie.write('xUSERNAME', user.username, {duration: 30});
	$('signin').setStyle('display', 'none');
	$('main').setStyle('display', 'block');
	
	view_show('dashboard');
	var ui = $('dashboard').empty();
	(new Element('li', {html: 'Use-cases'})).inject(ui);
	
	if (user.usecases.length == 0)
	{
		var li = (new Element('li')).inject(ui);
	   var a  = (new Element('a')).inject(li);
	   var d  = (new Element('div')).inject(a);
	   (new Element('span', {html: ''})).inject(d);	
      (new Element('big',  {html: 'No use-cases'})).inject(d);	
      (new Element('span', {html: 'You can create a new use-case! &#8594;'})).inject(d);
      li.addEvent('click', function(e) {ui_new_usecase()})
	} else {
		user.usecases.each(
			function(uc) {
			   ui_li_usecase(uc).inject(ui);
			   new Request.JSON({
	            url: '/' + user.username + '/process/' + uc.pid,
	            onSuccess: ui_li_process
	         }).get();
		   }
		)
	}	
}

function ui_li_usecase(uc)
{
   var li = (new Element('li', {id: uc.pid}));
	var a  = (new Element('a')).inject(li);
	//URI level results
	(new Element('small')).inject(a)
	                      .set('unit', 'ms')
	                      .set('scale', 1000)
	                      .setValue(0, 'PENDING');
	//Icon
	var icn = (new Element('img')).inject(a)
	//icn.src = uc.tm[lid]['icon.image'];
	
	//URI summary
   var d  = (new Element('div')).inject(a);
	(new Element('span', {html: uc.service})).inject(d);	
   (new Element('big',  {html: uc.usecase})).inject(d);	
   (new Element('span', {html: uc.script})).inject(d);	
   li.addEvent('click', function(e) {
   	pid = uc.pid;	
      if (e.target == icn)
         get_telemetry(pid, ui_graph);
      else
      	get_telemetry(pid, ui_telemetry);
   });
   return li;	
}

/*
%%
%%  Use-case
%%
*/
function put_usecase()
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

	if (valid && user)
	{
		new Request.JSON({
			url: '/' + user.username + '/process',
			onSuccess: get_user_profile
		}).send({
		   method: 'post',
		   data: JSON.encode({
		      service   : $('fservice').value,
		      usecase   : $('fusecase').value,
		      script    : $('furi').value,
		      thinktime : parseInt($('fthinktime').value),
		      http      : [ "User-Agent:" + $('fua').value ]
		   })
      });		
   }	
}

function put_existed_usecase()
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

	if (valid && user)
	{
		new Request.JSON({
		   emulation: false,	
	      url: '/' + user.username + '/process/' + pid,
	      onSuccess: function() {
	      	new Request.JSON({
			      url: '/' + user.username + '/process',
			      onSuccess: get_user_profile
		      }).send({
		         method: 'post',
		         data: JSON.encode({
		            service   : $('fservice').value,
		            usecase   : $('fusecase').value,
		            script    : $('furi').value,
		            thinktime : parseInt($('fthinktime').value),
		            http      : [ "User-Agent:" + $('fua').value ]
		         })
            });
	      }
	   }).delete();
	}
}

function remove_usecase()
{
	new Request.JSON({
		emulation: false,	
	   url: '/' + user.username + '/process/' + pid,
	   onSuccess: get_user_profile
	}).delete();
}


function ui_new_usecase()
{
	view_show('editor');
}

function ui_edit_usecase()
{
	view_show('editor');
	$('action-add').setStyle('display', 'none');
	$('action-del').setStyle('display', 'block');
	$('action-update').setStyle('display', 'block');
	
	var uc = null;
	user.usecases.each(function(u) {
	   if (u.pid == pid)
	      uc = u;
	});
	
	$('fservice').value = uc.service;
	$('fusecase').value = uc.usecase;
	
	$('furi').value = proc[pid].script;
	$('fua').value = proc[pid].http[0].substring(11);
	$('fthinktime').value = proc[pid].thinktime;
}



function ui_li_process(p)
{
	proc[p.id] = p;
	var li = $(p.id);
	li.getElement('small').setValue(p.tm[lid].uri, 'PENDING');
	li.getElement('img').src = p.tm[lid]['icon.image'];
	li.getElements('span')[1].innerHTML = p.script;
}

/*
%%
%% Telemetry
%%
 */
function get_telemetry(pid, next)
{
	new Request.JSON({
		url: '/' + user.username + '/telemetry/' + pid,
		onFailure: function() { alert('Unable to connect backend. Try again later.') },
		onSuccess: next
	}).get()
} 

function ui_telemetry(p)
{
	proc[p.id] = p;
	view_show('viewer');
	
	$('li-script').getElement('span').innerHTML = p.script;
	$('li-http').getElement('p').innerHTML = p.http;
	$('li-thinktime').getElement('span').innerHTML = p.thinktime;
	
	$('li-uri').getElement('span').setValue(p.tm[lid].uri, 'PENDING');
	$('li-dns').getElement('span').setValue(p.tm[lid].dns);
	$('li-tcp').getElement('span').setValue(p.tm[lid].tcp);
	$('li-ssl').getElement('span').setValue(p.tm[lid].ssl);
	$('li-ttfb').getElement('span').setValue(p.tm[lid].ttfb);
	$('li-ttmr').getElement('span').setValue(p.tm[lid].ttmr);
	$('li-size').getElement('span').setValue(p.tm[lid].size);
	$('li-pckt-size').getElement('span').setValue(p.tm[lid].chnk);
	$('li-pckt').getElement('span').setValue(p.tm[lid].pckt);
	
	$('li-response').getElement('pre').innerHTML = p.tm[lid].response.escapeHTML();
	$('li-payload').getElement('pre').innerHTML = p.tm[lid].payload.escapeHTML();
}

function ui_graph(p)
{
	proc = p;
	view_show('graph');
	/* scale */
	var sel = $('tscale');
	var opt = sel.options[ sel.selectedIndex ];
	var scale = '/' + opt.getAttribute('value') + '.png';
	var ui = $('graph').getElement('.images').empty();
	var schema = uri(p.id).schema;
	
	
	
	(new Element('img')).inject(ui).request(p.tm[lid]['latency.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['uri-' + schema + '.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['dns.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['tcp.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['ssl.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['ttfb.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['ttmr.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['error.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['size.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['pckt.image'] + scale);
	(new Element('img')).inject(ui).request(p.tm[lid]['pckt-cnt.image'] + scale);
}

/*
%%
%%  Cluster
%%
*/
function get_cluster()
{
   new Request.JSON({
      url: '/cluster',
         onFailure: function() { alert('Unable to connect backend. Try again later.') },
   	   onSuccess: ui_list_cluster
   	}).get();
}

function ui_list_cluster(list)
{
	if (list.length == 0)
	{
		alert('Cluster is not configured.');
		return;
	}
	
	var ui = $('cluster');
	ui.empty();
	(new Element('li', {html: 'Nodes'})).inject(ui);
	
	list.each(
	   function(node)
	   {
	   	var e = (new Element('li', {id: node.id})).inject(ui);
	   	if (!lid)
	   	{
	   		e.addClass('selected');
	   		lid = node.id;
	   	};
	   	var a = (new Element('a')).inject(e);
	   	var d  = (new Element('div')).inject(a);
	      (new Element('span', {html: ''})).inject(d);	
         (new Element('big',  {html: node.title})).inject(d);	
         (new Element('span', {html: node.id})).inject(d);	
	   	e.addEvent('click', function(e) {
	   		$(lid).toggleClass('selected');	
   	      lid = node.id;	
   	      $(lid).toggleClass('selected');	
   	      ui_list_processes(plist);
         });
	   }
	);
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
	view = id;
   $$('.panel').each(function(v){
   	if (v.getProperty('focused') == 'true')
   		stack.push(v.getProperty('id'));
      v.setProperty('focused', 'false')
   });
	$(id).setProperty('focused', 'true');	
	toolbar();
}

function view_back()
{
	var id = stack.pop();
	$$('.panel').each(function(v){
      v.setProperty('focused', 'false')
   });
	$(id).setProperty('focused', 'true');
	view = id;
	toolbar();
}

function toolbar()
{
	if (view == null)
	{
		$('back').setStyle('display', 'none');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'none');
		$('action-edit').setStyle('display', 'none');
		$('action-update').setStyle('display', 'none');
	}
	
	if (view == 'dashboard')
	{
		$('back').setStyle('display', 'none');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'block');
		$('action-edit').setStyle('display', 'none');
		$('action-update').setStyle('display', 'none');
	}
	
	if (view == 'viewer')
	{
		$('back').setStyle('display', 'block');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'block');
		$('action-add').setStyle('display', 'none');
		$('action-edit').setStyle('display', 'block');
		$('action-update').setStyle('display', 'none');
	}
	
	if (view == 'graph')
	{
		$('back').setStyle('display', 'block');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'none');
		$('action-edit').setStyle('display', 'none');
		$('action-update').setStyle('display', 'none');
	}
	
	if (view == 'editor')
	{
		$('back').setStyle('display', 'block');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'block');
		$('action-edit').setStyle('display', 'none');
		$('action-update').setStyle('display', 'none');
	}
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


//-------------------------------------------------------------------
//
// std classes extensions
//
//-------------------------------------------------------------------
String.prototype.escapeHTML = function () {                                        
        return(                                                                 
            this.replace(/&/g,'&amp;').                                         
                replace(/>/g,'&gt;').                                           
                replace(/</g,'&lt;').                                           
                replace(/"/g,'&quot;')                                         
        );                                                                      
    };

Element.prototype.request = function(uri) {
	if (this.get('tag') == 'img')
	{
		this.setStyle('display', 'none');
		var loader = new Image();
		loader.addEvent('load', function() {
		   this.setStyle('display', 'block').src = uri;
		}.bind(this));
		loader.src = uri;
	}
};    
  
Element.prototype.setValue = function(val, def) {
	var unit  = this.get('unit');
	unit = unit ? ' ' + unit : '';
	
	var scale = this.get('scale');
	scale = scale ? scale : 1;
	
	if (val && val != 0)
		this.innerHTML = (val / scale).toFixed(3) + unit;
	else if (def)
		this.innerHTML = def;
	else
		this.getParent().setStyle('display', 'none');
}



