package com.yrh.pattern.decorator_pattern.src;

/**
 * TShirt 类
 * @author Yrh
 *
 */
public class TShirts extends Finery {

	@Override
	public void show() {
		super.show();
		System.out.println("T血衫");
	}
}
