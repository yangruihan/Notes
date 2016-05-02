package com.yrh.pattern.chain_of_responsibility_pattern.src;

/**
 * 抽象处理类
 * @author Yrh
 *
 */
public abstract class Handler {
	protected Handler successor; // 继任者

	/**
	 * 设置继任者
	 * @param successor 继任者
	 */
	public void setSuccessor(Handler successor) {
		this.successor = successor;
	}
	
	/**
	 * 处理请求的抽象方法
	 * @param request 请求
	 */
	public abstract void handleRequest(int request);
}
