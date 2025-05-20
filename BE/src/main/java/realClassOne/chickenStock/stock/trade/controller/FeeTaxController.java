package realClassOne.chickenStock.stock.trade.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import realClassOne.chickenStock.stock.dto.response.FeeTaxSummaryResponseDTO;
import realClassOne.chickenStock.stock.trade.service.FeeTaxService;

@RestController
@RequestMapping("/api/admin/fee-tax")
@RequiredArgsConstructor
@Slf4j
public class FeeTaxController {

    private final FeeTaxService feeTaxService;

    @GetMapping("/summary")
    public ResponseEntity<FeeTaxSummaryResponseDTO> getSummary() {
        FeeTaxSummaryResponseDTO summary = feeTaxService.getSummary();
        return ResponseEntity.ok(summary);
    }
}