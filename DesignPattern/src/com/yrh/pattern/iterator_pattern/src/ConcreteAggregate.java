package com.yrh.pattern.iterator_pattern.src;

import java.util.ArrayList;
import java.util.List;

/**
 * 具体聚集类
 * @author Yrh
 *
 */
public class ConcreteAggregate extends Aggregate {

	private List<Object> items = new ArrayList<>();
	
	@Override
	public Iterator createIterator() {
		return new ConcreteIterator(this);
	}

	public int getCount() {
		return items.size();
	}
	
	public Object get(int index) {
		return items.get(index);
	}
	
	public void add(int index, Object o) {
		items.add(index, o);
	}
}
