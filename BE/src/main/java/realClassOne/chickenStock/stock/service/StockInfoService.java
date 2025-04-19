package realClassOne.chickenStock.stock.service;

import com.opencsv.CSVReader;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import realClassOne.chickenStock.stock.dto.common.StockResponse;
import realClassOne.chickenStock.stock.entity.Stock;
import realClassOne.chickenStock.stock.repository.StockRepository;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
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

    private final StockRepository stockRepository;

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
            long stockCount = stockRepository.count();
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

            log.info("주식 종목 정보 로드 완료: {} 종목", stockRepository.count());
        } catch (Exception e) {
            log.error("주식 종목 정보 로드 중 오류 발생", e);
        }
    }

//    private void loadStocksFromFile(File file) {
//        try (BufferedReader reader = new BufferedReader(
//                new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
//
//            // 첫 줄은 헤더이므로 스킵
//            String line = reader.readLine();
//            log.debug("헤더 라인: {}", line);
//
//            List<Stock> stockBatch = new ArrayList<>();
//            int count = 0;
//
//            while ((line = reader.readLine()) != null) {
//                try {
//                    // CSV 형식 파싱 (따옴표로 감싸진 값들 처리)
//                    String[] fields = parseCsvLine(line);
//
//                    // 필드 개수가 충분한지 확인
//                    if (fields.length >= 11) {
//                        String rawShortCode = fields[1].trim();
//                        String shortName = fields[3].trim();
//                        String market = fields[6].trim();
//                        String stockType = (fields.length > 9 && !fields[9].isEmpty()) ? fields[9].trim() : "일반";
//                        String faceValue = fields[10].trim();
//
//                        log.debug("파싱된 값 - 코드: {}, 이름: {}, 시장: {}, 유형: {}, 액면가: {}",
//                                rawShortCode, shortName, market, stockType, faceValue);
//
//                        // 단축코드를 6자리로 표준화
//                        String shortCode = normalizeShortCode(rawShortCode);
//
//                        Stock stock = Stock.builder()
//                                .shortCode(shortCode)
//                                .shortName(shortName)
//                                .market(market)
//                                .stockType(stockType)
//                                .faceValue(faceValue)
//                                .build();
//
//                        stockBatch.add(stock);
//                        count++;
//
//                        // 배치 크기가 100이 되면 저장
//                        if (stockBatch.size() >= 100) {
//                            stockRepository.saveAll(stockBatch);
//                            stockBatch.clear();
//                        }
//                    } else {
//                        log.warn("필드 수가 부족한 라인: {}", line);
//                    }
//                } catch (Exception e) {
//                    log.error("라인 처리 중 오류 발생: {}", line, e);
//                }
//            }
//
//            // 남은 배치 저장
//            if (!stockBatch.isEmpty()) {
//                stockRepository.saveAll(stockBatch);
//            }
//
//            log.info("{}에서 로드된 종목 수: {}", file.getName(), count);
//        } catch (Exception e) {
//            log.error("주식 종목 정보 파일 읽기 중 오류 발생: {}", file.getName(), e);
//            e.printStackTrace();
//        }
//    }

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
        try (InputStreamReader isr = new InputStreamReader(new FileInputStream(file), "EUC-KR"); // 인코딩 수정
             CSVReader csvReader = new CSVReader(isr)) {

            csvReader.skip(1); // 헤더 스킵

            List<Stock> stockBatch = new ArrayList<>();
            String[] fields;
            while ((fields = csvReader.readNext()) != null) {
                try {
                    if (fields.length >= 12) {
                        String shortCode = fields[1].trim();
                        String shortName = fields[3].trim();
                        String market = fields[6].trim();
                        String stockType = fields[9].trim().isEmpty() ? "일반" : fields[9].trim();
                        String faceValue = fields[10].trim();

                        // 단축코드 6자리 보정
                        shortCode = String.format("%06d", Integer.parseInt(shortCode));

                        Stock stock = Stock.builder()
                                .shortCode(shortCode)
                                .shortName(shortName)
                                .market(market)
                                .stockType(stockType)
                                .faceValue(faceValue)
                                .build();

                        stockBatch.add(stock);
                    }
                } catch (Exception e) {
                    log.error("CSV 라인 오류: {}", Arrays.toString(fields), e);
                }
            }

            stockRepository.saveAll(stockBatch);
            log.info("{}에서 로드된 종목 수: {}", file.getName(), stockBatch.size());

        } catch (Exception e) {
            log.error("파일 읽기 실패: {}", file.getAbsolutePath(), e);
        }
    }

    @Transactional(readOnly = true)
    public List<StockResponse> getAllStocks() {
        return stockRepository.findAll().stream()
                .map(this::mapStockToResponse)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public StockResponse getStockByCode(String code) {
        String normalizedCode = normalizeShortCode(code);
        return stockRepository.findById(normalizedCode)
                .map(this::mapStockToResponse)
                .orElse(null);
    }

    @Transactional(readOnly = true)
    public StockResponse getStockByName(String name) {
        return stockRepository.findByShortName(name)
                .map(this::mapStockToResponse)
                .orElse(null);
    }

    private StockResponse mapStockToResponse(Stock stock) {
        return StockResponse.builder()
                .shortCode(stock.getShortCode())
                .shortName(stock.getShortName())
                .market(stock.getMarket())
                .stockType(stock.getStockType())
                .faceValue(stock.getFaceValue())
                .build();
    }
}