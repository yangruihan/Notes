package com.yrh.pattern.simple_factory_pattern.src.operation;

/**
 * 加法运算类
 * @author Yrh
 *
 */
public class OperationAdd extends Operation {

	@Override
	public double getResult() {
		double result;
		result = getNumberA() + getNumberB();
		return result;
	}
}
