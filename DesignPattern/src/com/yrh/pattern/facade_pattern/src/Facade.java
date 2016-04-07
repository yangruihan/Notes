package com.yrh.pattern.facade_pattern.src;

/**
 * 外观类
 * @author Yrh
 *
 */
public class Facade {

	SubSystemOne one;
	SubSystemTwo two;
	SubSystemThree three;
	
	public Facade() {
		one = new SubSystemOne();
		two = new SubSystemTwo();
		three = new SubSystemThree();
	}
	
	public void methodA() {
		one.methodA();
		two.methodB();
	}
	
	public void methodB() {
		one.methodA();
		three.methodC();
	}
	
	public void methodAll() {
		one.methodA();
		two.methodB();
		three.methodC();
	}
}
