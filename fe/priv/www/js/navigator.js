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
Navigator = new Class({
	Implements  : Events,
		
	ui       : null, 
	items    : [],   // multi-dim array of items	
	                 // item[level] = [{id: ..., title: ...} ...]
	selected	: [],   
	
	/**
	 * Initialize the navigation object 
	 */
	initialize: function(list) 
	{
		this.ui = list;
	},
	
	/**
	 * Update the navigation with data
	 * @param {object} data
	 */
	 setItems: function(level, list)
	 {
	 	 this.items[level]  = list;
	 	 this.createUI(level);
	 },
	 
	 resetItems: function(level)
	 {
	 	 this.items[level]  = [];
	 	 $('navi' + level).empty();
	 },
	
	/**
	 * Create navigation DOM for given navi levels
	 * @param {array} levels
	 */
	createUI: function(level)
	{
		var ui = this.ui[level].empty(); //parent div
		// level selector
		var select = new Element('select', {id: 'nav' + level})
		             .inject(ui)
		             .addEvent('change', this.handleChange.bind(this)); //select
		this.items[level].each(
			function(item)
			{
				select.add(
					new Element(
						'option',
						{
							rel:  item.id,
							text: item.title
						}
					),
					null
				);
		   }.bind(this)
		);
		this.selected[level] = this.items[level][0].id;
	},
	 
	handleChange: function(e)
	{
		var level = e.target.getAttribute('id').substring(3);
		this.selected = [];
		for (var i = 0; i <= level; i++)
		{
		    var sel = $('nav' + i);
		    var opt = sel.options[ sel.selectedIndex ];
		    this.selected.push(opt.getAttribute('rel'));
		}
		this.fireEvent('change', this.selected);
	},
	
	getSelected: function()
	{
		return this.selected;
	},
	
	select: function(values)
	{
		this.selected = [];
		for (var i = 0; i < values.length; i++)
		{
			var sel = $('selnav' + i);
			// lookup item
			var itm = this.items[i].filter(function(item){ return item.sys_id == values[i] });
			if (itm.length > 0)
			{
				var ind = this.items[i].indexOf(itm[0]);
				sel.selectedIndex = ind;
				this.selected.push(values[i]);
				$('navi' + i).setStyle('display', 'inline-block'); //make sure the div is visible
			} else {
				// not found return selected path so far
				this.fireEvent('change', this);	
				return;
			}	
		}
		for (var i=values.length; i<this.items.length; i++) {
			$('navi' + i).setStyle('display', 'none'); //hide rest of the navigation
		}
		this.fireEvent('change', this);	
	}
});