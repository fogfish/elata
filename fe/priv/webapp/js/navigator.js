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
elata.Navigator = new Class({
	Implements  : Events,
		
	titles   : [],	
	items    : [],   // multi-dim array of items	
	selected	: [],   
	
	/**
	 * Initialize the navigation object 
	 */
	initialize: function() 
	{
	},
	
	/**
	 * Update the navigation with data
	 * @param {object} data
	 */
	 setItems: function(level, title, list)
	 {
	 	 this.titles[level] = title;
	 	 this.items[level]  = list;
	 	 this.createUI(level);
	 },
	 
	 resetItems: function(level)
	 {
	 	 this.titles[level] = null;
	 	 this.items[level]  = [];
	 	 $('navi' + level).empty();
	 },
	
	/**
	 * Create navigation DOM for given navi levels
	 * @param {array} levels
	 */
	createUI: function(level)
	{
		var ui = $('navi' + level).empty(); //parent div
		// level title
		new Element(
			'span', 
			{
				'class': 'label',
				text: this.titles[level]
			}
		).inject(ui);	
	   
		// level selector
		var el     = new Element('span', {'class': 'item'}).inject(ui); //wrapper
		var select = new Element('select', {id: 'selnav' + level}).inject(el).addEvent('change', this.handleChange.bind(this)); //select
		this.items[level].each(
			function(item)
			{
				select.add(
					new Element(
						'option',
						{
							rel:  item.sys_id,
							text: item.title
						}
					),
					null
				);
		   }.bind(this)
		);
	},
	 
	handleChange: function(e)
	{
		var level = e.target.getAttribute('id').substring(6);
		this.selected = [];
		for (var i = 0; i <= level; i++)
		{
		    var sel = $('selnav' + i);
		    var opt = sel.options[ sel.selectedIndex ];
		    this.selected.push(opt.getAttribute('rel'));
		}
		this.fireEvent('change', this);
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