package com.yrh.pattern.decorator_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		
		Person p = new Person("小明");
		
		TShirts tShirts = new TShirts();
		Jeans jeans = new Jeans();
		BoardShoes boardShoes = new BoardShoes();
		
		tShirts.decorate(p);
		jeans.decorate(tShirts);
		boardShoes.decorate(jeans);
		
		boardShoes.show();
	}
}
