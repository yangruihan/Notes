package com.yrh.pattern.strategy_pattern.src;

/**
 * 正常收费类
 * @author Yrh
 *
 */
public class CashNormal extends CashSuper {
	
	double money;
	
	public CashNormal(double money) {
		this.money = money;
	}

	@Override
	public double acceptCash() {
		return this.money;
	}
}
