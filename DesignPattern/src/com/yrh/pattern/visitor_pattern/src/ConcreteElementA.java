package com.yrh.pattern.visitor_pattern.src;

/**
 * 具体元素类A
 * @author Yrh
 *
 */
public class ConcreteElementA extends Element {
	@Override
	public void accept(Visitor visitor) {
		visitor.visitConcreteElementA(this);
	}
	
	/**
	 * 其他相关方法
	 */
	public void operationA() {
	}
}
