package com.yrh.pattern.template_method_pattern.src;

/**
 * 抽象类
 * @author Yrh
 *
 */
public abstract class AbstractClass {

	/**
	 * 在父类中将不变的行为进行封装
	 */
	public void templateMethod() {
		System.out.println("Template Method Begin!");
		
		// 调用可变的行为
		concreteMethodA();
		concreteMethodB();
		
		System.out.println("Template Method End!");
	}
	
	/**
	 * 留出方法来给子类对具体可变的行为进行修改和扩展
	 */
	protected abstract void concreteMethodA();
	
	protected abstract void concreteMethodB();
}
