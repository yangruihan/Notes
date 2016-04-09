package com.yrh.pattern.decorator_pattern.src;

/**
 * 牛仔裤类
 * @author Yrh
 *
 */
public class Jeans extends Finery {

	@Override
	public void show() {
		super.show();
		System.out.println("牛仔裤");
	}
}
