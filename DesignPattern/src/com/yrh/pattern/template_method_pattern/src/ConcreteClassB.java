package com.yrh.pattern.template_method_pattern.src;

/**
 * 具体实现细节类B
 * @author Yrh
 *
 */
public class ConcreteClassB extends AbstractClass {

	@Override
	public void concreteMethodA() {
		System.out.println("ConcreteClassB : concreteMethodA");
	}

	@Override
	public void concreteMethodB() {
		System.out.println("ConcreteClassB : concreteMethodB");
	}

}
