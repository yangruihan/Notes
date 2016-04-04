package com.yrh.pattern.simple_factory_pattern.src.operation;

/**
 * 次方运算类
 * @author Yrh
 *
 */
public class OperationPow extends Operation {

	@Override
	public double getResult() {
		double result;
		result = Math.pow(getNumberA(), getNumberB());
		return result;
	}
}
