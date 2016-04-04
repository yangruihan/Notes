package com.yrh.pattern.strategy_pattern.src;

import java.util.Scanner;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		
		System.out.println("请输入收费类型：");
		String type = scan.next();
		
		System.out.println("请输入单价：");
		double price = scan.nextDouble();
		
		System.out.println("请输入数量：");
		double number = scan.nextDouble();
		
		CashContext cashContext = new CashContext(type, price * number);
		
		System.out.println("应付金额为：" + cashContext.getResult());
	}
}
