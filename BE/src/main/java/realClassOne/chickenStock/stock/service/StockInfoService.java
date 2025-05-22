package realClassOne.chickenStock.stock.service;

import com.opencsv.CSVReader;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.common.exception.CustomException;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.entity.StockData;
import realClassOne.chickenStock.stock.exception.StockErrorCode;
import realClassOne.chickenStock.stock.repository.StockDataRepository;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Slf4j
@RequiredArgsConstructor
public class StockInfoService {

    @Value("${app.stock.csv-path}")
    private String csvPath;

    private final StockDataRepository stockDataRepository;

    @PostConstruct
    public void loadStockData() {
        try {
            // 파일 경로 설정
            File stock1File = new File(csvPath, "stock1.csv");
            File stock2File = new File(csvPath, "stock2.csv");

            log.info("파일 경로 확인 - stock1: {}", stock1File.getAbsolutePath());
            log.info("파일 존재 여부 - stock1: {}", stock1File.exists());
            log.info("파일 경로 확인 - stock2: {}", stock2File.getAbsolutePath());
            log.info("파일 존재 여부 - stock2: {}", stock2File.exists());

            // 기존 데이터가 있는지 확인
            long stockCount = stockDataRepository.count();
            if (stockCount > 0) {
                log.info("이미 DB에 {}개의 종목 정보가 있습니다. CSV 로드를 건너뜁니다.", stockCount);
                return;
            }

            // CSV 파일 로드
            if(stock1File.exists()) {
                loadStocksFromFile(stock1File);
            } else {
                log.error("stock1.csv 파일을 찾을 수 없습니다.");
            }

            if(stock2File.exists()) {
                loadStocksFromFile(stock2File);
            } else {
                log.error("stock2.csv 파일을 찾을 수 없습니다.");
            }

            log.info("주식 종목 정보 로드 완료: {} 종목", stockDataRepository.count());
        } catch (Exception e) {
            log.error("주식 종목 정보 로드 중 오류 발생", e);
        }
    }

    // CSV 라인 파싱 (따옴표로 감싸진 값 처리)
    private String[] parseCsvLine(String line) {
        List<String> tokens = new ArrayList<>();
        boolean inQuotes = false;
        StringBuilder currentToken = new StringBuilder();

        for (char c : line.toCharArray()) {
            if (c == '\"') {
                inQuotes = !inQuotes;
            } else if (c == ',' && !inQuotes) {
                tokens.add(currentToken.toString().trim());
                currentToken.setLength(0);
            } else {
                currentToken.append(c);
            }
        }
        tokens.add(currentToken.toString().trim()); // 마지막 토큰 추가

        return tokens.toArray(new String[0]);
    }

    private String normalizeShortCode(String code) {
        code = code.trim();
        // 숫자만 있는 경우 6자리로 포맷 (앞의 0 유지)
        if (code.matches("^\\d+$")) {
            return String.format("%06d", Integer.parseInt(code));
        }
        return code;
    }

    private void loadStocksFromFile(File file) {
        try (InputStreamReader isr = new InputStreamReader(new FileInputStream(file), "EUC-KR");
             CSVReader csvReader = new CSVReader(isr)) {

            csvReader.skip(1); // 헤더 스킵

            List<StockData> stockDataBatches = new ArrayList<>();
            String[] fields;
            while ((fields = csvReader.readNext()) != null) {
                try {
                    if (fields.length >= 12) {
                        String shortCode = fields[1].trim();
                        String shortName = fields[3].trim();
                        String market = fields[6].trim();
                        String stockType = fields[9].trim().isEmpty() ? "일반" : fields[9].trim();
                        String faceValue = fields[10].trim();

                        // 단축코드 정규화 (문자 포함된 코드도 처리)
                        shortCode = normalizeShortCodeWithChars(shortCode);

                        StockData stockData = StockData.of(
                                shortCode,
                                shortName,
                                market,
                                stockType,
                                faceValue
                        );

                        stockDataBatches.add(stockData);
                    }
                } catch (Exception e) {
                    log.error("CSV 라인 오류: {}", Arrays.toString(fields), e);
                }
            }

            stockDataRepository.saveAll(stockDataBatches);
            log.info("{}에서 로드된 종목 수: {}", file.getName(), stockDataBatches.size());

        } catch (Exception e) {
            log.error("파일 읽기 실패: {}", file.getAbsolutePath(), e);
        }
    }

    // 문자가 포함된 단축코드도 처리하는 메소드
    private String normalizeShortCodeWithChars(String code) {
        code = code.trim();
        // 숫자만 있는 경우
        if (code.matches("^\\d+$")) {
            return String.format("%06d", Integer.parseInt(code));
        }
        // 숫자와 문자가 혼합된 경우 (예: "00104K")
        else {
            // 숫자 부분과 문자 부분 분리
            String numPart = code.replaceAll("[^0-9]", "");
            String charPart = code.replaceAll("[0-9]", "");

            // 숫자 부분을 0으로 패딩
            int digitCount = 6 - charPart.length();
            if (digitCount > 0) {
                numPart = String.format("%0" + digitCount + "d", Integer.parseInt(numPart));
            }

            return numPart + charPart;
        }
    }

    @Transactional(readOnly = true)
    public List<StockResponse> getAllStocks() {
        return stockDataRepository.findAll().stream()
                .map(this::mapStockToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public StockResponse getStockByCode(String code) {
        String normalizedCode = normalizeShortCode(code);
        return stockDataRepository.findById(normalizedCode)
                .map(this::mapStockToResponse)
                .orElseThrow(() -> new CustomException(StockErrorCode.STOCK_NOT_FOUND));
    }

    private StockResponse mapStockToResponse(StockData stockData) {
        return StockResponse.builder()
                .shortCode(stockData.getShortCode())
                .shortName(stockData.getShortName())
                .market(stockData.getMarket())
                .stockType(stockData.getStockType())
                .faceValue(stockData.getFaceValue())
                .build();
    }
}