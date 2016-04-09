package com.yrh.pattern.simple_factory_pattern.src.operation;

/**
 * 乘法运算类
 * @author Yrh
 *
 */
public class OperationMul extends Operation {

	@Override
	public double getResult() {
		double result = 0;
		result = getNumberA() * getNumberB();
		return result;
	}
}
