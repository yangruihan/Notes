package com.yrh.pattern.strategy_pattern.src;

/**
 * 打折收费
 * @author Yrh
 *
 */
public class CashRebate extends CashSuper {

	double rate; // 折扣率
	double money;
	
	public CashRebate(double money, double rate) {
		this.money = money;
		this.rate = rate;
	}

	@Override
	public double acceptCash() {
		return money * rate;
	}
}
