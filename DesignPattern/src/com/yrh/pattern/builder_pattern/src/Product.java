package com.yrh.pattern.builder_pattern.src;

import java.util.ArrayList;
import java.util.List;

/**
 * 产品类
 * 
 * @author Yrh
 *
 */
public class Product {

	List<String> parts = new ArrayList<String>();

	public void add(String part) {
		parts.add(part);
	}

	public void show() {
		System.out.println("\n产品 创建 ----");
		for (String s : parts) {
			System.out.println(s);
		}
	}
}
