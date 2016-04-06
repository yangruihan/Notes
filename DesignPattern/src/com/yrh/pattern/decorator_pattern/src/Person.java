package com.yrh.pattern.decorator_pattern.src;

/**
 * 人类
 * @author Yrh
 *
 */
public class Person {

	public Person() {
	}
	
	private String name;

	public Person(String name) {
		super();
		this.name = name;
	}
	
	public void show() {
		System.out.println("装扮的" + name);
	}
}
