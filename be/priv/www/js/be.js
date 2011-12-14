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
var pid     = null;  //active process
var lid     = null;  //active location

var plist   = null;
var proc    = null;

var ui_list = null;
var stack   = [];
var view    = null;

//
//
//
String.prototype.escapeHTML = function () {                                        
        return(                                                                 
            this.replace(/&/g,'&amp;').                                         
                replace(/>/g,'&gt;').                                           
                replace(/</g,'&lt;').                                           
                replace(/"/g,'&quot;')                                         
        );                                                                      
    };

//
//
//
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

    
function main()
{
	ui_list = $('dashboard');
	
	// /* add new use-case */
	$('action-add').addEvent('click', function(e) {
      e.preventDefault();
      if (view == 'editor')
		   put_process();
		else
		   ui_new_proc();
	});
	// /* remove use-case */
	$('action-del').addEvent('click', function(e) {
	   e.preventDefault();
	   remove_process();
	});
	// /* switch to statistic view */
	$('action-stat').addEvent('click', function(e){
	   e.preventDefault();
	   ui_graph(proc);
	});
	/* switch scale */
	$('tscale').addEvent('change', function(e){
	   ui_graph(proc);
	});
	 
	$('back').addEvent('click', function(e){
		e.preventDefault();	
	   view_back();	
	});
	toolbar();
	get_cluster();
}


/*
%%
%% Get list of processes
%%
*/
function get_processes()
{
   new Request.JSON({
      url: '/process',
         onFailure: function() { alert('Unable to connect backend. Try again later.') },
   	   onSuccess: ui_list_processes
   	}).get();
}

function get_process(pid, next)
{
	new Request.JSON({
		url: '/process/' + pid,
		onFailure: function() { alert('Unable to connect backend. Try again later.') },
		onSuccess: next
	}).get()
}

function put_process()
{
   var valid = true;
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

	if (valid)
	{
		new Request.JSON({
			url: '/process',
			onSuccess: get_processes
		}).send({
		   method: 'post',
		   data: JSON.encode({
		      script    : $('furi').value,
		      thinktime : parseInt($('fthinktime').value),
		      http      : [ "User-Agent: " + $('fua').value ]
		   })
      });		
   }		
}


function remove_process()
{
	new Request.JSON({
		emulation: false,	
	   url: '/process/' + pid,
	   onSuccess: get_processes
	}).delete();
}


function get_cluster()
{
   new Request.JSON({
      url: '/cluster',
         onFailure: function() { alert('Unable to connect backend. Try again later.') },
   	   onSuccess: ui_list_cluster
   	}).get();
}


function ui_list_processes(list)
{
	plist = list;
   view_show('dashboard');
	ui_list.empty();
	(new Element('li', {html: 'Measuments'})).inject(ui_list);
	
	
	list.each(
	   function(proc)
	   {
	      ui_li_process(proc).inject(ui_list)	
	   }
	);
}


function ui_li_process(p)
{
	var li = (new Element('li', {id: p.id}));
	var a  = (new Element('a')).inject(li);
	//URI level results
	(new Element('small')).inject(a)
	                      .set('unit', 'ms')
	                      .set('scale', 1000)
	                      .setValue(p.tm[lid].uri, 'PENDING');
	//Icon
	var icn = (new Element('img')).inject(a)
	icn.src = p.tm[lid]['icon.image'];
	
	//URI summary
   var d  = (new Element('div')).inject(a);
	(new Element('span', {html: ''})).inject(d);	
   (new Element('big',  {html: p.script})).inject(d);	
   (new Element('span', {html: p.http[0]})).inject(d);	
   li.addEvent('click', function(e) {
   	pid = p.id;	
      if (e.target == icn)
         get_process(p.id, ui_graph);
      else
      	get_process(p.id, ui_proc)
   });
   return li;
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


function ui_proc(p)
{
	proc = p;
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


function ui_new_proc()
{
	view_show('editor');
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
	get_processes();
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
	}
	
	if (view == 'dashboard')
	{
		$('back').setStyle('display', 'none');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'block');
	}
	
	if (view == 'viewer')
	{
		$('back').setStyle('display', 'block');
		$('action-del').setStyle('display', 'block');
		$('action-stat').setStyle('display', 'block');
		$('action-add').setStyle('display', 'none');
	}
	
	if (view == 'graph')
	{
		$('back').setStyle('display', 'block');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'none');
	}
	
	if (view == 'editor')
	{
		$('back').setStyle('display', 'block');
		$('action-del').setStyle('display', 'none');
		$('action-stat').setStyle('display', 'none');
		$('action-add').setStyle('display', 'block');
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




