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
elata.Grid = new Class({

	initialize : function(ui, vbar, hbar) {
		// this.c = (new Element('div', {id: 'grid'})) $();
		this.c = $(ui);
		this.vbar = $(vbar).getElement('div');
		this.hbar = $(hbar);

		this.items = [];
		this.row = -1;
		this.col = -1;
		// this.c.addEvent('click', this.nextItem.bind(this));
		this.selectedCol = 0;
		this.selectedRow = 0;
	},

	reset : function() {
		this.items = [];
		this.c.empty();
		this.vbar.empty();
		this.hbar.empty();
		this.row = -1;
		this.col = 0;
	},

	addRow : function() {
		this.col = 0;
		this.row++;
		this.items[this.row] = [];
	},

	addItem : function(item) {
		item.fx = new Fx.Morph(item, {
			unit : '%',
			link : 'chain'
		})
		item.fx.set({
			'left' : -100
		}); // invisible
		this.items[this.row][this.col] = item;
		this.col++;
	},

	showItem : function(row, col) {
		if (this.items.length > 0) {
			this.row = (row < this.items.length) ? row : this.row.length - 1;
			this.col = (col < this.items[this.row].length) ? col
					: this.col.length - 1;
			this.items[this.row][this.col].fx.set({
				'left' : 0,
				'top' : 0
			});
			this.updateVbar();
			this.updateHbar();
		}
	},

	nextCol : function() {
		if (this.col < this.items[this.row].length - 1) {
			// slide out current
			this.items[this.row][this.col].fx.start({
				'left' : [ 0, -100 ],
				'top' : [ 0, 0 ]
			});
			this.col++;
			// slide in new
			this.items[this.row][this.col].fx.start({
				'left' : [ 100, 0 ],
				'top' : [ 0, 0 ]
			});
			this.updateHbar();
		}
	},

	prevCol : function() {
		if (this.col > 0) {
			// slide out current
			this.items[this.row][this.col].fx.start({
				'left' : [ 0, 100 ],
				'top' : [ 0, 0 ]
			});
			this.col--;
			// slide in new
			this.items[this.row][this.col].fx.start({
				'left' : [ -100, 0 ],
				'top' : [ 0, 0 ]
			});
			this.updateHbar();
		}
	},

	prevRow : function() {
		if (this.row > 0) {
			// slide out current
			this.items[this.row][this.col].fx.start({
				'left' : [ 0, 0 ],
				'top' : [ 0, 100 ]
			});
			this.row--;
			if (this.col >= this.items[this.row].length)
				this.col = this.items[this.row].length - 1;
			this.items[this.row][this.col].fx.start({
				'left' : [ 0, 0 ],
				'top' : [ -100, 0 ]
			});
			this.updateVbar();
		}
	},

	nextRow : function() {
		if (this.row < this.items.length - 1) {
			// slide out current
			this.items[this.row][this.col].fx.start({
				'left' : [ 0, 0 ],
				'top' : [ 0, -100 ]
			});
			this.row++;
			if (this.col >= this.items[this.row].length)
				this.col = this.items[this.row].length - 1;
			this.items[this.row][this.col].fx.start({
				'left' : [ 0, 0 ],
				'top' : [ 100, 0 ]
			});
			this.updateVbar();
		}
	},

	updateVbar : function() {
		// update vbar / hbar
		this.vbar.innerHTML = '';
		for ( var i = 0; i < this.row; i++)
			this.vbar.innerHTML += '<div>&#x25c7;</div>';
		this.vbar.innerHTML += '<div>&#x25c6;</div>';
		for ( var i = this.row + 1; i < this.items.length; i++)
			this.vbar.innerHTML += '<div>&#x25c7;</div>';
		this.selectedRow = this.row;
	},

	updateHbar : function() {
		// update vbar / hbar
		this.hbar.innerHTML = '';
		for ( var i = 0; i < this.col; i++)
			this.hbar.innerHTML += '<span>&#x25c7;</span>';
		this.hbar.innerHTML += '<span>&#x25c6;</span>';
		for ( var i = this.col + 1; i < this.items[this.row].length; i++)
			this.hbar.innerHTML += '<span>&#x25c7;</span>';
		this.selectedCol = this.col;
	}

});
