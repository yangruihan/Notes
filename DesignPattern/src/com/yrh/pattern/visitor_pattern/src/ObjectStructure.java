package com.yrh.pattern.visitor_pattern.src;

import java.util.ArrayList;
import java.util.List;

/**
 * 对象结构类
 * @author Yrh
 *
 */
public class ObjectStructure {
	private List<Element> elements = new ArrayList<>();
	
	public void attach(Element e) {
		elements.add(e);
	}
	
	public void detach(Element e) {
		elements.remove(e);
	}
	
	public void accept(Visitor visitor) {
		for (Element e : elements){
			e.accept(visitor);
		}
	}
}
