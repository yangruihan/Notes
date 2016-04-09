package com.yrh.pattern.template_method_pattern.src;

/**
 * 具体实现细节类A
 * @author Yrh
 *
 */
public class ConcreteClassA extends AbstractClass {

	@Override
	public void concreteMethodA() {
		System.out.println("ConcreteClassA : concreteMethodA");
	}

	@Override
	public void concreteMethodB() {
		System.out.println("ConcreteClassA : concreteMethodB");
	}

}
