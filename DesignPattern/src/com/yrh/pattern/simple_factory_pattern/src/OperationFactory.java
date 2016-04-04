package com.yrh.pattern.simple_factory_pattern.src;

import com.yrh.pattern.simple_factory_pattern.src.operation.Operation;
import com.yrh.pattern.simple_factory_pattern.src.operation.OperationAdd;
import com.yrh.pattern.simple_factory_pattern.src.operation.OperationDiv;
import com.yrh.pattern.simple_factory_pattern.src.operation.OperationMul;
import com.yrh.pattern.simple_factory_pattern.src.operation.OperationPow;
import com.yrh.pattern.simple_factory_pattern.src.operation.OperationSub;

/**
 * 运算符工厂
 * @author Yrh
 *
 */
public class OperationFactory {

	/**
	 * 根据传入运算符创建相应的运算类
	 * @param operate 运算符
	 * @return 运算类
	 */
	public static Operation createOperate(String operate) {
		Operation oper = null;
		switch (operate) {
		case "+":
			oper = new OperationAdd();
			break;
		case "-":
			oper = new OperationSub();
			break;
		case "*":
			oper = new OperationMul();
			break;
		case "/":
			oper = new OperationDiv();
			break;
		case "^":
			oper = new OperationPow();
			break;
		default:
			break;
		}
		return oper;
	}
	
}
