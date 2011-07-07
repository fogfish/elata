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
elata.kvset = {
	
	decode: function(tuple)
	{
		if (instanceOf(tuple, Array))
		{
			var list = [];
			tuple.each(
				function(item)
				{
					list.push(elata.kvset.__decode_root(item));
				}
			);
			return list;
		} else {
			return elata.kvset.__decode_root(tuple);
		}
	},

	
	
	encode: function(json)
	{
		var data = [];
		for (k in json) {
			if (k != "sys_id") {
				data.push( this.__encode_tuple(k, json[k]) );
			}
		}
		if (json.sys_id) {
			data = [
	             new BertAtom("kvset"), 
	             new BertBinary(json.sys_id), 
	             data
             ];
         return new BertTuple(data);    
		} else {
			return data;
		}
	},
	
	
	__encode_tuple: function(key, value) 
	{
		var data = [];
		if (instanceOf(value, Array)) {
			data.push( new BertBinary(key) );
			data.push( elata.kvset.__encode_array(value) );
		} else if (instanceOf(value, Object)) {
			//special case where key is actually binary
			//data.push( new BertBinary(key) );
			var obj = elata.kvset.encode(value);
			/*
			var list = []; //loop tuples into list
			for ( var i = 0; i < obj[0].length; i++) {
				list.push( obj[0][i] );
			}
			data.push( list );
			*/
			return new BertTuple([new BertBinary(key), obj]);
		} else {
			data.push( new BertBinary(key) );
			data.push( new BertBinary(value) );
		}
		return new BertTuple(data);
	},
	
	
	__encode_array: function(array)
	{
		var list = [];
		for ( var i = 0; i < array.length; i++) {
			list.push( elata.kvset.__encode_tuple(array[i]) );
		}
		return list;
	},
	
	
	
	__decode_root: function(tuple)
	{
		var obj = {};
		
		//assert input
		if ((tuple[0][0].type != "atom") && (tuple[0][0].value != "kvset"))
			return obj;
		
		obj.sys_id = tuple[0][1].value;
		
		tuple[0][2].each(
			function(item)
			{
				var key  = item[0][0].value;
				obj[key] = elata.kvset.__decode_tuple(item[0][1]);
			}
		);
		
		return obj;		
	},
	
	__decode_tuple: function(tuple)
	{
		if (instanceOf(tuple, Array))
		{
			//check the types of array items
			if (instanceOf(tuple[0], BertTuple))
			{
				//array of key-val pairs
				var obj = {};
			   tuple.each(
				   function(item)
				   {
					   var key  = item[0][0].value;
				      obj[key] = elata.kvset.__decode_tuple(item[0][1]);
				   }
			   );
			   return obj;
			} else {
				//array of primitive types
				var list = []
				tuple.each(
					function(item)
					{
						if (instanceOf(item, BertAtom))
			            list.push(item.value);
		            else if (instanceOf(tuple, BertBinary))
			            list.push(item.value);
		            else if (instanceOf(tuple, BertTuple))
		            	list.push(tuple.value[0]);
		            else 
						   list.push(item);
					}
				);
				return list;
			}
		};
		
		if (instanceOf(tuple, BertAtom))
			return tuple.value;
		if (instanceOf(tuple, BertBinary))
			return tuple.value;
		if (instanceOf(tuple, BertTuple))
			return tuple.value[0];
		
		return tuple;
	}
}
