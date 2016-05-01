package com.yrh.pattern.visitor_pattern.src;

/**
 * 具体访问者类1
 * 
 * @author Yrh
 *
 */
public class ConcreteVisitor1 extends Visitor {

	@Override
	public void visitConcreteElementA(ConcreteElementA concreteElementA) {
		System.out.println(concreteElementA.getClass().getName() + " 被 " + this.getClass().getName() + " 访问");
	}

	@Override
	public void visitConcreteElementB(ConcreteElementB concreteElementB) {
		System.out.println(concreteElementB.getClass().getName() + " 被 " + this.getClass().getName() + " 访问");
	}
}
