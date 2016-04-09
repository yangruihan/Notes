package com.yrh.pattern.simple_factory_pattern.src.operation;

/**
 * 除法运算类
 * @author Yrh
 *
 */
public class OperationDiv extends Operation {

	@Override
	public double getResult() {
		double result = 0;
		if (getNumberB() == 0) {
			throw new ArithmeticException("除数不能为0！");
		}
		result = getNumberA() / getNumberB();
		return result;
	}
}
