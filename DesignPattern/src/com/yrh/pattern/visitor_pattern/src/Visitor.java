package com.yrh.pattern.visitor_pattern.src;

/**
 * 访问者抽象类
 * @author Yrh
 *
 */
public abstract class Visitor {
	public abstract void visitConcreteElementA(ConcreteElementA concreteElementA);
	
	public abstract void visitConcreteElementB(ConcreteElementB concreteElementB);
}
