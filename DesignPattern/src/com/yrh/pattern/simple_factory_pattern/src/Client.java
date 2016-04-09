package com.yrh.pattern.simple_factory_pattern.src;

import java.util.Scanner;

import com.yrh.pattern.simple_factory_pattern.src.operation.Operation;

/**
 * 客户端类 
 * @author Yrh
 *
 */
public class Client {
	
	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		
		System.out.print("请输入第一个操作数：");
		double numberA = scan.nextDouble();
		
		System.out.print("请输入操作符：");
		String oper = scan.next();
		
		System.out.print("请输入第二个操作数：");
		double numberB = scan.nextDouble();
		
		Operation operation = OperationFactory.createOperate(oper);
		operation.setNumberA(numberA);
		operation.setNumberB(numberB);
		double result = operation.getResult();
		
		System.out.println("结果为：" + result);
	}
}
