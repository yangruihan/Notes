package com.yrh.pattern.decorator_pattern.src;

/**
 * 服饰类
 * @author Yrh
 *
 */
public class Finery extends Person {

	protected Person component;
	
	/**
	 * 打扮
	 * @param component
	 */
	public void decorate(Person component) {
		this.component = component;
	}
	
	@Override
	public void show() {
		if (component != null) {
			component.show();
		}
	}
}
