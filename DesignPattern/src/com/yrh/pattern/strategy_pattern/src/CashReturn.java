package com.yrh.pattern.strategy_pattern.src;

/**
 * 返利收费类
 * @author Yrh
 *
 */
public class CashReturn extends CashSuper {
	
	double moneyCondition; // 返现条件
	double moneyReturn; // 返现金额
	double money;
	
	public CashReturn(double money, double moneyCondition, double moneyReturn) {
		this.money = money;
		this.moneyCondition = moneyCondition;
		this.moneyReturn = moneyReturn;
	}

	@Override
	public double acceptCash() {
		return money - (int)(money / moneyCondition) * moneyReturn; 
	}

}
