package com.yrh.pattern.simple_factory_pattern.src.operation;

/**
 * 运算抽象类
 * @author Yrh
 *
 */
public abstract class Operation {
	private double numberA = 0;
	private double numberB = 0;
	
	public abstract double getResult();
	
	/* get && set */
	public double getNumberA() {
		return numberA;
	}
	public void setNumberA(double numberA) {
		this.numberA = numberA;
	}
	public double getNumberB() {
		return numberB;
	}
	public void setNumberB(double numberB) {
		this.numberB = numberB;
	}
}
