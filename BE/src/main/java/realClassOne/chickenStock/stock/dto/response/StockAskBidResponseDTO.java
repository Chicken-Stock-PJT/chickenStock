package realClassOne.chickenStock.stock.dto.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StockAskBidResponseDTO {
    // 기본 정보
    @JsonProperty("bid_req_base_tm")
    private String bidReqBaseTm;  // 호가잔량기준시간

    // 매도 호가 정보 (10차 ~ 1차)
    @JsonProperty("sel_10th_pre_req")
    private String sel10thPreReq;     // 매도10차선잔량

    @JsonProperty("sel_10th_pre_bid")
    private String sel10thPreBid;     // 매도10차선호가

    @JsonProperty("sel_9th_pre_req")
    private String sel9thPreReq;      // 매도9차선잔량

    @JsonProperty("sel_9th_pre_bid")
    private String sel9thPreBid;      // 매도9차선호가

    @JsonProperty("sel_8th_pre_req")
    private String sel8thPreReq;      // 매도8차선잔량

    @JsonProperty("sel_8th_pre_bid")
    private String sel8thPreBid;      // 매도8차선호가

    @JsonProperty("sel_7th_pre_req")
    private String sel7thPreReq;      // 매도7차선잔량

    @JsonProperty("sel_7th_pre_bid")
    private String sel7thPreBid;      // 매도7차선호가

    @JsonProperty("sel_6th_pre_req")
    private String sel6thPreReq;      // 매도6차선잔량

    @JsonProperty("sel_6th_pre_bid")
    private String sel6thPreBid;      // 매도6차선호가

    @JsonProperty("sel_5th_pre_req")
    private String sel5thPreReq;      // 매도5차선잔량

    @JsonProperty("sel_5th_pre_bid")
    private String sel5thPreBid;      // 매도5차선호가

    @JsonProperty("sel_4th_pre_req")
    private String sel4thPreReq;      // 매도4차선잔량

    @JsonProperty("sel_4th_pre_bid")
    private String sel4thPreBid;      // 매도4차선호가

    @JsonProperty("sel_3th_pre_req")
    private String sel3thPreReq;      // 매도3차선잔량

    @JsonProperty("sel_3th_pre_bid")
    private String sel3thPreBid;      // 매도3차선호가

    @JsonProperty("sel_2th_pre_req")
    private String sel2thPreReq;      // 매도2차선잔량

    @JsonProperty("sel_2th_pre_bid")
    private String sel2thPreBid;      // 매도2차선호가

    @JsonProperty("sel_fpr_req")
    private String selFprReq;         // 매도최우선잔량

    @JsonProperty("sel_fpr_bid")
    private String selFprBid;         // 매도최우선호가

    // 매수 호가 정보 (1차 ~ 10차)
    @JsonProperty("buy_fpr_bid")
    private String buyFprBid;         // 매수최우선호가

    @JsonProperty("buy_fpr_req")
    private String buyFprReq;         // 매수최우선잔량

    @JsonProperty("buy_2th_pre_bid")
    private String buy2thPreBid;      // 매수2차선호가

    @JsonProperty("buy_2th_pre_req")
    private String buy2thPreReq;      // 매수2차선잔량

    @JsonProperty("buy_3th_pre_bid")
    private String buy3thPreBid;      // 매수3차선호가

    @JsonProperty("buy_3th_pre_req")
    private String buy3thPreReq;      // 매수3차선잔량

    @JsonProperty("buy_4th_pre_bid")
    private String buy4thPreBid;      // 매수4차선호가

    @JsonProperty("buy_4th_pre_req")
    private String buy4thPreReq;      // 매수4차선잔량

    @JsonProperty("buy_5th_pre_bid")
    private String buy5thPreBid;      // 매수5차선호가

    @JsonProperty("buy_5th_pre_req")
    private String buy5thPreReq;      // 매수5차선잔량

    @JsonProperty("buy_6th_pre_bid")
    private String buy6thPreBid;      // 매수6차선호가

    @JsonProperty("buy_6th_pre_req")
    private String buy6thPreReq;      // 매수6차선잔량

    @JsonProperty("buy_7th_pre_bid")
    private String buy7thPreBid;      // 매수7차선호가

    @JsonProperty("buy_7th_pre_req")
    private String buy7thPreReq;      // 매수7차선잔량

    @JsonProperty("buy_8th_pre_bid")
    private String buy8thPreBid;      // 매수8차선호가

    @JsonProperty("buy_8th_pre_req")
    private String buy8thPreReq;      // 매수8차선잔량

    @JsonProperty("buy_9th_pre_bid")
    private String buy9thPreBid;      // 매수9차선호가

    @JsonProperty("buy_9th_pre_req")
    private String buy9thPreReq;      // 매수9차선잔량

    @JsonProperty("buy_10th_pre_bid")
    private String buy10thPreBid;     // 매수10차선호가

    @JsonProperty("buy_10th_pre_req")
    private String buy10thPreReq;     // 매수10차선잔량

    // 총 잔량 정보
    @JsonProperty("tot_sel_req")
    private String totSelReq;         // 총매도잔량

    @JsonProperty("tot_buy_req")
    private String totBuyReq;         // 총매수잔량

    // 응답 코드
    @JsonProperty("return_code")
    private int returnCode;

    @JsonProperty("return_msg")
    private String returnMsg;
}