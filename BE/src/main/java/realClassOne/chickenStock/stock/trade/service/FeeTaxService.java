package realClassOne.chickenStock.stock.trade.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import realClassOne.chickenStock.stock.dto.response.FeeTaxSummaryResponseDTO;
import realClassOne.chickenStock.stock.entity.FeeTaxSummary;
import realClassOne.chickenStock.stock.repository.FeeTaxSummaryRepository;

@Service
@RequiredArgsConstructor
@Slf4j
public class FeeTaxService {

    private final FeeTaxSummaryRepository feeTaxSummaryRepository;

    // 매수 수수료 계산 (0.015%)
    public long calculateBuyFee(long amount) {
        return Math.round(amount * 0.00015);
    }

    // 매도 수수료 계산 (0.015%)
    public long calculateSellFee(long amount) {
        return Math.round(amount * 0.00015);
    }

    // 매도 세금 계산 (0.18%)
    public long calculateSellTax(long amount) {
        return Math.round(amount * 0.0018);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Retryable(value = {OptimisticLockingFailureException.class},
            maxAttempts = 3,
            backoff = @Backoff(delay = 100))
    public void addBuyFee(long fee) {
        try {
            FeeTaxSummary summary = getOrCreateSummary();
            summary.addBuyFee(fee);
            feeTaxSummaryRepository.save(summary);
            log.debug("매수 수수료 {} 원 추가 완료", fee);
        } catch (Exception e) {
            log.error("매수 수수료 기록 실패: {} 원", fee, e);
            // 수수료 기록 실패는 거래 자체에 영향을 주지 않도록 예외를 전파하지 않음
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Retryable(value = {OptimisticLockingFailureException.class},
            maxAttempts = 3,
            backoff = @Backoff(delay = 100))
    public void addSellFee(long fee) {
        try {
            FeeTaxSummary summary = getOrCreateSummary();
            summary.addSellFee(fee);
            feeTaxSummaryRepository.save(summary);
            log.debug("매도 수수료 {} 원 추가 완료", fee);
        } catch (Exception e) {
            log.error("매도 수수료 기록 실패: {} 원", fee, e);
            // 수수료 기록 실패는 거래 자체에 영향을 주지 않도록 예외를 전파하지 않음
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Retryable(value = {OptimisticLockingFailureException.class},
            maxAttempts = 3,
            backoff = @Backoff(delay = 100))
    public void addSellTax(long tax) {
        try {
            FeeTaxSummary summary = getOrCreateSummary();
            summary.addSellTax(tax);
            feeTaxSummaryRepository.save(summary);
            log.debug("매도 세금 {} 원 추가 완료", tax);
        } catch (Exception e) {
            log.error("매도 세금 기록 실패: {} 원", tax, e);
            // 세금 기록 실패는 거래 자체에 영향을 주지 않도록 예외를 전파하지 않음
        }
    }

    @Transactional(readOnly = true)
    public FeeTaxSummaryResponseDTO getSummary() {
        FeeTaxSummary summary = getOrCreateSummary();
        return FeeTaxSummaryResponseDTO.fromEntity(summary);
    }

    private FeeTaxSummary getOrCreateSummary() {
        return feeTaxSummaryRepository.findLatest()
                .orElseGet(() -> {
                    FeeTaxSummary newSummary = FeeTaxSummary.create();
                    return feeTaxSummaryRepository.save(newSummary);
                });
    }

    // 여러 수수료/세금을 한 번에 추가하는 메서드 (트랜잭션 효율성 향상)
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    @Retryable(value = {OptimisticLockingFailureException.class},
            maxAttempts = 3,
            backoff = @Backoff(delay = 100))
    public void addFeesAndTaxes(Long buyFee, Long sellFee, Long sellTax) {
        try {
            FeeTaxSummary summary = getOrCreateSummary();

            if (buyFee != null && buyFee > 0) {
                summary.addBuyFee(buyFee);
            }
            if (sellFee != null && sellFee > 0) {
                summary.addSellFee(sellFee);
            }
            if (sellTax != null && sellTax > 0) {
                summary.addSellTax(sellTax);
            }

            feeTaxSummaryRepository.save(summary);
            log.debug("수수료/세금 일괄 추가 완료 - 매수수수료: {}, 매도수수료: {}, 매도세금: {}",
                    buyFee, sellFee, sellTax);
        } catch (Exception e) {
            log.error("수수료/세금 일괄 기록 실패", e);
        }
    }
}