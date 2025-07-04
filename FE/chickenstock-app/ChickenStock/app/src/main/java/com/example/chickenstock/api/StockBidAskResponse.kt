package com.example.chickenstock.api

data class StockBidAskResponse(
    val bid_req_base_tm: String,
    val sel_10th_pre_req: String,
    val sel_10th_pre_bid: String,
    val sel_9th_pre_req: String,
    val sel_9th_pre_bid: String,
    val sel_8th_pre_req: String,
    val sel_8th_pre_bid: String,
    val sel_7th_pre_req: String,
    val sel_7th_pre_bid: String,
    val sel_6th_pre_req: String,
    val sel_6th_pre_bid: String,
    val sel_5th_pre_req: String,
    val sel_5th_pre_bid: String,
    val sel_4th_pre_req: String,
    val sel_4th_pre_bid: String,
    val sel_3th_pre_req: String,
    val sel_3th_pre_bid: String,
    val sel_2th_pre_req: String,
    val sel_2th_pre_bid: String,
    val sel_fpr_req: String,
    val sel_fpr_bid: String,
    val buy_fpr_bid: String,
    val buy_fpr_req: String,
    val buy_2th_pre_bid: String,
    val buy_2th_pre_req: String,
    val buy_3th_pre_bid: String,
    val buy_3th_pre_req: String,
    val buy_4th_pre_bid: String,
    val buy_4th_pre_req: String,
    val buy_5th_pre_bid: String,
    val buy_5th_pre_req: String,
    val buy_6th_pre_bid: String,
    val buy_6th_pre_req: String,
    val buy_7th_pre_bid: String,
    val buy_7th_pre_req: String,
    val buy_8th_pre_bid: String,
    val buy_8th_pre_req: String,
    val buy_9th_pre_bid: String,
    val buy_9th_pre_req: String,
    val buy_10th_pre_bid: String,
    val buy_10th_pre_req: String,
    val tot_sel_req: String,
    val tot_buy_req: String,
    val return_code: Int,
    val return_msg: String
) 