package com.yrh.pattern.visitor_pattern.src;

/**
 * 具体元素类B
 * @author Yrh
 *
 */
public class ConcreteElementB extends Element{

	@Override
	public void accept(Visitor visitor) {
		visitor.visitConcreteElementB(this);
	}
	
	/**
	 * 其他相关方法
	 */
	public void operationB() {
	}
}
