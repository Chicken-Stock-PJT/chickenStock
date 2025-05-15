
    // 전역 변수
    let stocksData = [];
    let botsData = [];
    let summaryData = {};
    let signalsData = {};
    let envelopeChart = null;
    let bollingerChart = null;
    let envelopeSignalsChart = null;
    let bollingerSignalsChart = null;
    let stockPriceChart = null;
    let botSignalsCharts = {};

    // 유틸리티 함수: 숫자 포맷
    function formatNumber(num) {
        if (num === undefined || num === null) return 'N/A';
        
        return new Intl.NumberFormat('ko-KR').format(num);
    }

    // 유틸리티 함수: 신호 포맷
    function formatSignal(signal) {
        if (!signal) return '<span class="badge bg-secondary">없음</span>';
        
        let color = 'secondary';
        
        if (signal === '매수') {
            color = 'success';
        } else if (signal === '매도') {
            color = 'danger';
        } else if (signal === '중립') {
            color = 'info';
        }
        
        return `<span class="badge bg-${color}">${signal}</span>`;
    }

    // 유틸리티 함수: 전략별 색상
    function getStrategyColor(strategy) {
        switch (strategy) {
            case 'ENVELOPE':
                return 'success';
            case 'BOLLINGER':
                return 'info';
            case 'SHORT_TERM':
                return 'warning';
            default:
                return 'secondary';
        }
    }

    // 페이지 로드 시 실행
    document.addEventListener('DOMContentLoaded', () => {
        // 초기 데이터 로드
        loadAllData();
        
        // 새로고침 버튼 이벤트
        document.getElementById('refresh-btn').addEventListener('click', loadAllData);
        
        // 네비게이션 이벤트
        setupNavigation();
        
        // 종목 검색 이벤트
        document.getElementById('stock-search-btn').addEventListener('click', filterStocks);
        document.getElementById('stock-search').addEventListener('keyup', (e) => {
            if (e.key === 'Enter') {
                filterStocks();
            }
        });
        
        // 자동 새로고침 설정 (30초마다)
        setInterval(loadAllData, 30000);
    });

    // 모든 데이터 로드
    function loadAllData() {
        loadCacheSummary();
        loadFilteredStocks();
        loadBotsInfo();
        loadSignalsSummary();
        
        // 마지막 업데이트 시간 표시
        const now = new Date();
        document.getElementById('last-update').textContent = `마지막 업데이트: ${now.toLocaleTimeString()}`;
    }

    // 네비게이션 설정
    function setupNavigation() {
        const navLinks = document.querySelectorAll('.nav-link');
        navLinks.forEach(link => {
            link.addEventListener('click', (e) => {
                e.preventDefault();
                
                // 활성 링크 스타일 변경
                navLinks.forEach(l => l.classList.remove('active'));
                link.classList.add('active');
                
                // 해당 섹션 표시
                const targetSection = link.getAttribute('data-section');
                document.querySelectorAll('.section-content').forEach(section => {
                    section.classList.add('d-none');
                });
                document.getElementById(`${targetSection}-section`).classList.remove('d-none');
            });
        });
    }

    // 캐시 요약 정보 로드
    async function loadCacheSummary() {
        try {
            const response = await fetch('/monitor/cache_summary');
            if (!response.ok) throw new Error('캐시 요약 정보를 불러오는데 실패했습니다.');
            
            summaryData = await response.json();
            
            // 요약 정보 표시
            const cacheSummary = document.getElementById('cache-summary');
            cacheSummary.innerHTML = `
                <div class="row">
                    <div class="col-md-6">
                        <ul class="list-group">
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                총 종목 수
                                <span class="badge bg-primary rounded-pill">${summaryData.total_stocks}</span>
                            </li>
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                필터링된 종목 수
                                <span class="badge bg-success rounded-pill">${summaryData.filtered_stocks}</span>
                            </li>
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                일봉 차트 종목 수
                                <span class="badge bg-info rounded-pill">${summaryData.chart_data_stocks}</span>
                            </li>
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                분봉 차트 종목 수
                                <span class="badge bg-warning rounded-pill">${summaryData.minute_chart_stocks}</span>
                            </li>
                        </ul>
                    </div>
                    <div class="col-md-6">
                        <ul class="list-group">
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                Envelope 지표 계산 종목 수
                                <span class="badge bg-success rounded-pill">${summaryData.envelope_stocks}</span>
                            </li>
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                볼린저 지표 계산 종목 수
                                <span class="badge bg-info rounded-pill">${summaryData.bollinger_stocks}</span>
                            </li>
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                현재가 캐싱 종목 수
                                <span class="badge bg-primary rounded-pill">${summaryData.price_cache_stocks}</span>
                            </li>
                            <li class="list-group-item d-flex justify-content-between align-items-center">
                                실시간 구독 종목 수
                                <span class="badge bg-danger rounded-pill">${summaryData.subscribed_stocks}</span>
                            </li>
                        </ul>
                    </div>
                </div>
            `;
            
            // 활성 봇 요약 정보 표시
            const botsCount = summaryData.bot_count || 0;
            const activeBotsSummary = document.getElementById('active-bots-summary');
            
            if (botsCount > 0 && summaryData.bots) {
                let botsHtml = `
                    <div class="alert alert-info">
                        실행 중인 봇 ${botsCount}개가 있습니다.
                    </div>
                    <div class="table-responsive">
                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th>이메일</th>
                                    <th>전략</th>
                                    <th>현금</th>
                                    <th>보유종목</th>
                                </tr>
                            </thead>
                            <tbody>
                `;
                
                summaryData.bots.forEach(bot => {
                    botsHtml += `
                        <tr>
                            <td>${bot.email}</td>
                            <td><span class="badge bg-info">${bot.strategy}</span></td>
                            <td>${formatNumber(bot.cash)}원</td>
                            <td>${bot.holdings_count}개</td>
                        </tr>
                    `;
                });
                
                botsHtml += `
                            </tbody>
                        </table>
                    </div>
                `;
                
                activeBotsSummary.innerHTML = botsHtml;
            } else {
                activeBotsSummary.innerHTML = `
                    <div class="alert alert-warning">
                        현재 실행 중인 봇이 없습니다.
                    </div>
                `;
            }
            
            // 차트 업데이트
            updateSignalCharts();
            
        } catch (error) {
            console.error('캐시 요약 정보 로드 오류:', error);
            document.getElementById('cache-summary').innerHTML = `
                <div class="alert alert-danger">
                    캐시 요약 정보를 불러오는데 실패했습니다: ${error.message}
                </div>
            `;
        }
    }

    // 필터링된 종목 목록 로드
    async function loadFilteredStocks() {
        try {
            const response = await fetch('/monitor/filtered_stocks');
            if (!response.ok) throw new Error('필터링된 종목 목록을 불러오는데 실패했습니다.');
            
            const data = await response.json();
            stocksData = data.stocks || [];
            
            // 테이블 업데이트
            renderStocksTable(stocksData);
            
        } catch (error) {
            console.error('필터링된 종목 목록 로드 오류:', error);
            document.querySelector('#stocks-table tbody').innerHTML = `
                <tr>
                    <td colspan="7" class="text-center">
                        <div class="alert alert-danger">
                            종목 목록을 불러오는데 실패했습니다: ${error.message}
                        </div>
                    </td>
                </tr>
            `;
        }
    }

    // 봇 정보 로드
    async function loadBotsInfo() {
        try {
            const response = await fetch('/monitor/bots');
            if (!response.ok) throw new Error('봇 정보를 불러오는데 실패했습니다.');
            
            const data = await response.json();
            botsData = data.bots || [];
            
            // 봇 카드 렌더링
            renderBotsCards(botsData);
            
        } catch (error) {
            console.error('봇 정보 로드 오류:', error);
            document.getElementById('bots-cards').innerHTML = `
                <div class="col-12">
                    <div class="alert alert-danger">
                        봇 정보를 불러오는데 실패했습니다: ${error.message}
                    </div>
                </div>
            `;
        }
    }

    // 신호 요약 정보 로드
    async function loadSignalsSummary() {
        try {
            const response = await fetch('/monitor/signals_summary');
            if (!response.ok) throw new Error('신호 요약 정보를 불러오는데 실패했습니다.');
            
            signalsData = await response.json();
            
            // 차트 업데이트
            updateSignalCharts();
            
            // 봇별 신호 차트 업데이트
            updateBotSignalsCharts();
            
        } catch (error) {
            console.error('신호 요약 정보 로드 오류:', error);
        }
    }

    // 종목 테이블 렌더링
    function renderStocksTable(stocks) {
        const tbody = document.querySelector('#stocks-table tbody');
        
        if (!stocks || stocks.length === 0) {
            tbody.innerHTML = `
                <tr>
                    <td colspan="7" class="text-center">필터링된 종목이 없습니다.</td>
                </tr>
            `;
            return;
        }
        
        let html = '';
        
        stocks.forEach(stock => {
            html += `
                <tr>
                    <td>${stock.code}</td>
                    <td>${stock.name}</td>
                    <td>${stock.market}</td>
                    <td>${formatNumber(stock.price)}</td>
                    <td>${formatSignal(stock.envelope_signal)}</td>
                    <td>${formatSignal(stock.bollinger_signal)}</td>
                    <td>
                        <button class="btn btn-sm btn-primary" onclick="showStockDetails('${stock.code}')">
                            상세보기
                        </button>
                    </td>
                </tr>
            `;
        });
        
        tbody.innerHTML = html;
    }

    // 봇 카드 렌더링
    function renderBotsCards(bots) {
        const botsContainer = document.getElementById('bots-cards');
        
        if (!bots || bots.length === 0) {
            botsContainer.innerHTML = `
                <div class="col-12">
                    <div class="alert alert-warning">
                        현재 실행 중인 봇이 없습니다.
                    </div>
                </div>
            `;
            return;
        }
        
        let html = '';
        
        bots.forEach(bot => {
            const strategyColor = getStrategyColor(bot.strategy);
            
            html += `
                <div class="col-md-6 mb-4">
                    <div class="card h-100">
                        <div class="card-header bg-${strategyColor} text-white">
                            <h5 class="mb-0">${bot.email}</h5>
                        </div>
                        <div class="card-body">
                            <div class="row mb-3">
                                <div class="col-md-4">
                                    <div class="card">
                                        <div class="card-body p-2 text-center">
                                            <h6 class="card-title">전략</h6>
                                            <span class="badge bg-${strategyColor}">${bot.strategy}</span>
                                        </div>
                                    </div>
                                </div>
                                <div class="col-md-4">
                                    <div class="card">
                                        <div class="card-body p-2 text-center">
                                            <h6 class="card-title">현금</h6>
                                            <p class="mb-0">${formatNumber(bot.cash)}원</p>
                                        </div>
                                    </div>
                                </div>
                                <div class="col-md-4">
                                    <div class="card">
                                        <div class="card-body p-2 text-center">
                                            <h6 class="card-title">보유종목</h6>
                                            <p class="mb-0">${bot.holdings_count}개</p>
                                        </div>
                                    </div>
                                </div>
                            </div>
                            
                            <h6>보유종목 목록</h6>
            `;
            
            if (bot.holdings && bot.holdings.length > 0) {
                html += `
                    <div class="table-responsive">
                        <table class="table table-sm table-striped">
                            <thead>
                                <tr>
                                    <th>종목코드</th>
                                    <th>종목명</th>
                                    <th>수량</th>
                                    <th>평균단가</th>
                                </tr>
                            </thead>
                            <tbody>
                `;
                
                bot.holdings.forEach(holding => {
                    html += `
                        <tr>
                            <td>${holding.symbol}</td>
                            <td>${holding.name || '-'}</td>
                            <td>${holding.quantity}</td>
                            <td>${formatNumber(holding.avgPrice)}원</td>
                        </tr>
                    `;
                });
                
                html += `
                            </tbody>
                        </table>
                    </div>
                `;
            } else {
                html += `
                    <div class="alert alert-light">
                        보유 중인 종목이 없습니다.
                    </div>
                `;
            }
            
            html += `
                        </div>
                        <div class="card-footer">
                            <small class="text-muted">
                                지표 데이터: Envelope ${bot.indicator_counts.envelope}개, 
                                볼린저 ${bot.indicator_counts.bollinger}개, 
                                단타 ${bot.indicator_counts.short_term}개
                            </small>
                        </div>
                    </div>
                </div>
            `;
        });
        
        botsContainer.innerHTML = html;
    }

    // 종목 검색 필터링
    function filterStocks() {
        const searchInput = document.getElementById('stock-search').value.toLowerCase();
        
        if (!searchInput) {
            renderStocksTable(stocksData);
            return;
        }
        
        const filteredStocks = stocksData.filter(stock => 
            stock.code.toLowerCase().includes(searchInput) || 
            stock.name.toLowerCase().includes(searchInput)
        );
        
        renderStocksTable(filteredStocks);
    }

    // 종목 상세 정보 표시
    async function showStockDetails(symbolCode) {
        try {
            const modal = new bootstrap.Modal(document.getElementById('stock-detail-modal'));
            
            // 로딩 메시지 표시
            document.getElementById('stock-detail-content').innerHTML = `
                <div class="text-center">
                    <div class="spinner-border text-primary" role="status">
                        <span class="visually-hidden">Loading...</span>
                    </div>
                    <p>종목 데이터를 불러오는 중...</p>
                </div>
            `;
            
            modal.show();
            
            // 종목 상세 정보 불러오기
            const response = await fetch(`/monitor/stock_details/${symbolCode}`);
            if (!response.ok) throw new Error('종목 상세 정보를 불러오는데 실패했습니다.');
            
            const stockDetail = await response.json();
            
            // 기본 정보 표시
            document.getElementById('stock-detail-content').innerHTML = `
                <div class="row">
                    <div class="col-md-6">
                        <h4>${stockDetail.name} (${stockDetail.symbol})</h4>
                        <p class="text-muted">${stockDetail.market}</p>
                    </div>
                    <div class="col-md-6 text-end">
                        <h4>${formatNumber(stockDetail.current_price)}원</h4>
                    </div>
                </div>
            `;
            
            // 차트 데이터 표시
            displayStockChart(stockDetail);
            
            // Envelope 지표 표시
            displayEnvelopeIndicators(stockDetail.envelope_indicator);
            
            // 볼린저 밴드 지표 표시
            displayBollingerIndicators(stockDetail.bollinger_indicator);
            
            // 봇별 지표 표시
            displayBotIndicators(stockDetail.bot_indicators);
            
        } catch (error) {
            console.error('종목 상세 정보 로드 오류:', error);
            document.getElementById('stock-detail-content').innerHTML = `
                <div class="alert alert-danger">
                    종목 상세 정보를 불러오는데 실패했습니다: ${error.message}
                </div>
            `;
        }
    }

    // 종목 차트 표시
    function displayStockChart(stockDetail) {
        const ctx = document.getElementById('stock-price-chart').getContext('2d');
        
        // 기존 차트 제거
        if (stockPriceChart) {
            stockPriceChart.destroy();
        }
        
        // 차트 데이터 준비
        const chartData = stockDetail.chart_data;
        
        if (!chartData || chartData.length === 0) {
            document.getElementById('stock-price-chart').parentNode.innerHTML = `
                <div class="alert alert-warning">
                    차트 데이터가 없습니다.
                </div>
            `;
            return;
        }
        
        // 데이터 포맷 변환
        let labels = [];
        let prices = [];
        
        // 데이터가 배열인지 객체인지 확인하여 처리
        chartData.forEach(item => {
            if (typeof item === 'object') {
                if (Array.isArray(item)) {
                    // 배열 형식인 경우 [date, open, high, low, close, volume]
                    labels.push(item[0]);
                    prices.push(item[4]); // close
                } else {
                    // 객체 형식인 경우 {date, open, high, low, close, volume}
                    labels.push(item.date);
                    prices.push(item.close);
                }
            }
        });
        
        // 라벨 역순 정렬 (최신 데이터가 오른쪽에 표시되도록)
        labels.reverse();
        prices.reverse();
        
        // 차트 생성
        stockPriceChart = new Chart(ctx, {
            type: 'line',
            data: {
                labels: labels,
                datasets: [{
                    label: '종가',
                    data: prices,
                    borderColor: 'rgb(75, 192, 192)',
                    backgroundColor: 'rgba(75, 192, 192, 0.2)',
                    tension: 0.1,
                    fill: true
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    title: {
                        display: true,
                        text: '최근 30일 차트'
                    },
                    tooltip: {
                        mode: 'index',
                        intersect: false,
                    }
                },
                scales: {
                    y: {
                        beginAtZero: false
                    }
                }
            }
        });
    }

    // Envelope 지표 표시
    function displayEnvelopeIndicators(indicator) {
        const container = document.getElementById('envelope-indicators');
        
        if (!indicator) {
            container.innerHTML = `
                <div class="alert alert-warning">
                    Envelope 지표 데이터가 없습니다.
                </div>
            `;
            return;
        }
        
        container.innerHTML = `
            <div class="table-responsive">
                <table class="table table-bordered">
                    <tbody>
                        <tr>
                            <th>상한선</th>
                            <td>${formatNumber(indicator.upperBand)}</td>
                        </tr>
                        <tr>
                            <th>중앙선 (MA20)</th>
                            <td>${formatNumber(indicator.middleBand)}</td>
                        </tr>
                        <tr>
                            <th>하한선</th>
                            <td>${formatNumber(indicator.lowerBand)}</td>
                        </tr>
                        <tr>
                            <th>현재가</th>
                            <td>${formatNumber(indicator.currentPrice)}</td>
                        </tr>
                        <tr>
                            <th>신호</th>
                            <td>${formatSignal(indicator.signal)}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        `;
    }

    // 볼린저 밴드 지표 표시
    function displayBollingerIndicators(indicator) {
        const container = document.getElementById('bollinger-indicators');
        
        if (!indicator) {
            container.innerHTML = `
                <div class="alert alert-warning">
                    볼린저 밴드 지표 데이터가 없습니다.
                </div>
            `;
            return;
        }
        
        container.innerHTML = `
            <div class="table-responsive">
                <table class="table table-bordered">
                    <tbody>
                        <tr>
                            <th>상한선</th>
                            <td>${formatNumber(indicator.upperBand)}</td>
                        </tr>
                        <tr>
                            <th>중앙선 (SMA)</th>
                            <td>${formatNumber(indicator.middleBand)}</td>
                        </tr>
                        <tr>
                            <th>하한선</th>
                            <td>${formatNumber(indicator.lowerBand)}</td>
                        </tr>
                        <tr>
                            <th>현재가</th>
                            <td>${formatNumber(indicator.currentPrice)}</td>
                        </tr>
                        <tr>
                            <th>%B</th>
                            <td>${indicator.percentB ? indicator.percentB.toFixed(2) : 'N/A'}</td>
                        </tr>
                        <tr>
                            <th>밴드폭</th>
                            <td>${indicator.bandwidth ? indicator.bandwidth.toFixed(4) : 'N/A'}</td>
                        </tr>
                        <tr>
                            <th>신호</th>
                            <td>${formatSignal(indicator.signal)}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        `;
    }

    // 봇별 지표 표시
    function displayBotIndicators(botIndicators) {
        const container = document.getElementById('bot-indicators');
        
        if (!botIndicators || Object.keys(botIndicators).length === 0) {
            container.innerHTML = `
                <div class="alert alert-warning">
                    봇별 지표 데이터가 없습니다.
                </div>
            `;
            return;
        }
        
        let html = '';
        
        Object.entries(botIndicators).forEach(([email, data]) => {
            const strategyColor = getStrategyColor(data.strategy);
            const indicators = data.indicators || {};
            
            html += `
                <div class="card mb-3">
                    <div class="card-header bg-${strategyColor} text-white">
                        <h6 class="mb-0">${email} (${data.strategy})</h6>
                    </div>
                    <div class="card-body">
            `;
            
            if (Object.keys(indicators).length > 0) {
                html += `
                    <div class="table-responsive">
                        <table class="table table-sm table-bordered">
                            <tbody>
                `;
                
                if (data.strategy === 'ENVELOPE') {
                    html += `
                        <tr>
                            <th>상한선</th>
                            <td>${formatNumber(indicators.upperBand)}</td>
                            <th>중앙선</th>
                            <td>${formatNumber(indicators.middleBand)}</td>
                        </tr>
                        <tr>
                            <th>하한선</th>
                            <td>${formatNumber(indicators.lowerBand)}</td>
                            <th>신호</th>
                            <td>${formatSignal(indicators.signal)}</td>
                        </tr>
                    `;
                } else if (data.strategy === 'BOLLINGER') {
                    html += `
                        <tr>
                            <th>상한선</th>
                            <td>${formatNumber(indicators.upperBand)}</td>
                            <th>중앙선</th>
                            <td>${formatNumber(indicators.middleBand)}</td>
                        </tr>
                        <tr>
                            <th>하한선</th>
                            <td>${formatNumber(indicators.lowerBand)}</td>
                            <th>%B</th>
                            <td>${indicators.percentB ? indicators.percentB.toFixed(2) : 'N/A'}</td>
                        </tr>
                        <tr>
                            <th>밴드폭</th>
                            <td>${indicators.bandwidth ? indicators.bandwidth.toFixed(4) : 'N/A'}</td>
                            <th>신호</th>
                            <td>${formatSignal(indicators.signal)}</td>
                        </tr>
                    `;
                } else if (data.strategy === 'SHORT_TERM') {
                    html += `
                        <tr>
                            <th>거래량 급증</th>
                            <td>${indicators.volume_surge ? '있음' : '없음'}</td>
                            <th>기준선</th>
                            <td>${formatNumber(indicators.ichimoku_baseline)}</td>
                        </tr>
                        <tr>
                            <th>신호</th>
                            <td colspan="3">${formatSignal(indicators.signal)}</td>
                        </tr>
                    `;
                }
                
                html += `
                            </tbody>
                        </table>
                    </div>
                `;
            } else {
                html += `
                    <div class="alert alert-light">
                        지표 데이터가 없습니다.
                    </div>
                `;
            }
            
            html += `
                    </div>
                </div>
            `;
        });
        
        container.innerHTML = html;
    }

    // 신호 차트 업데이트
    function updateSignalCharts() {
        // Envelope 차트 업데이트
        updateEnvelopeChart();
        
        // 볼린저 밴드 차트 업데이트
        updateBollingerChart();
        
        // 신호 분석 차트 업데이트
        if (signalsData && signalsData.envelope_signals) {
            updateSignalsDetailCharts();
        }
    }

    // Envelope 차트 업데이트
    function updateEnvelopeChart() {
        const ctx = document.getElementById('envelope-chart').getContext('2d');
        
        // 기존 차트 제거
        if (envelopeChart) {
            envelopeChart.destroy();
        }
        
        let envelopeData = [0, 0, 0];
        let labels = ['매수', '중립', '매도'];
        
        if (signalsData && signalsData.envelope_signals) {
            envelopeData = [
                signalsData.envelope_signals['매수'] || 0,
                signalsData.envelope_signals['중립'] || 0,
                signalsData.envelope_signals['매도'] || 0
            ];
        }
        
        envelopeChart = new Chart(ctx, {
            type: 'doughnut',
            data: {
                labels: labels,
                datasets: [{
                    data: envelopeData,
                    backgroundColor: [
                        'rgba(40, 167, 69, 0.7)',
                        'rgba(108, 117, 125, 0.7)',
                        'rgba(220, 53, 69, 0.7)'
                    ],
                    borderColor: [
                        'rgb(40, 167, 69)',
                        'rgb(108, 117, 125)',
                        'rgb(220, 53, 69)'
                    ],
                    borderWidth: 1
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    legend: {
                        position: 'bottom',
                    },
                    title: {
                        display: true,
                        text: 'Envelope 신호 분포'
                    }
                }
            }
        });
    }

    // 볼린저 밴드 차트 업데이트
    function updateBollingerChart() {
        const ctx = document.getElementById('bollinger-chart').getContext('2d');
        
        // 기존 차트 제거
        if (bollingerChart) {
            bollingerChart.destroy();
        }
        
        let bollingerData = [0, 0, 0];
        let labels = ['매수', '중립', '매도'];
        
        if (signalsData && signalsData.bollinger_signals) {
            bollingerData = [
                signalsData.bollinger_signals['매수'] || 0,
                signalsData.bollinger_signals['중립'] || 0,
                signalsData.bollinger_signals['매도'] || 0
            ];
        }
        
        bollingerChart = new Chart(ctx, {
            type: 'doughnut',
            data: {
                labels: labels,
                datasets: [{
                    data: bollingerData,
                    backgroundColor: [
                        'rgba(40, 167, 69, 0.7)',
                        'rgba(108, 117, 125, 0.7)',
                        'rgba(220, 53, 69, 0.7)'
                    ],
                    borderColor: [
                        'rgb(40, 167, 69)',
                        'rgb(108, 117, 125)',
                        'rgb(220, 53, 69)'
                    ],
                    borderWidth: 1
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    legend: {
                        position: 'bottom',
                    },
                    title: {
                        display: true,
                        text: '볼린저 밴드 신호 분포'
                    }
                }
            }
        });
    }

    // 신호 분석 차트 업데이트
    function updateSignalsDetailCharts() {
        updateEnvelopeSignalsChart();
        updateBollingerSignalsChart();
    }

    // Envelope 신호 상세 차트 업데이트
    function updateEnvelopeSignalsChart() {
        const ctx = document.getElementById('envelope-signals-chart').getContext('2d');
        
        // 기존 차트 제거
        if (envelopeSignalsChart) {
            envelopeSignalsChart.destroy();
        }
        
        // 데이터 준비
        const markets = Object.keys(signalsData.envelope_by_market || {});
        const buyData = [];
        const neutralData = [];
        const sellData = [];
        
        markets.forEach(market => {
            const marketData = signalsData.envelope_by_market[market];
            buyData.push(marketData['매수'] || 0);
            neutralData.push(marketData['중립'] || 0);
            sellData.push(marketData['매도'] || 0);
        });
        
        // 차트 생성
        envelopeSignalsChart = new Chart(ctx, {
            type: 'bar',
            data: {
                labels: markets,
                datasets: [
                    {
                        label: '매수',
                        data: buyData,
                        backgroundColor: 'rgba(40, 167, 69, 0.7)',
                        borderColor: 'rgb(40, 167, 69)',
                        borderWidth: 1
                    },
                    {
                        label: '중립',
                        data: neutralData,
                        backgroundColor: 'rgba(108, 117, 125, 0.7)',
                        borderColor: 'rgb(108, 117, 125)',
                        borderWidth: 1
                    },
                    {
                        label: '매도',
                        data: sellData,
                        backgroundColor: 'rgba(220, 53, 69, 0.7)',
                        borderColor: 'rgb(220, 53, 69)',
                        borderWidth: 1
                    }
                ]
            },
            options: {
                responsive: true,
                plugins: {
                    title: {
                        display: true,
                        text: '시장별 Envelope 신호 분포'
                    },
                    tooltip: {
                        mode: 'index',
                        intersect: false
                    }
                },
                scales: {
                    x: {
                        stacked: true,
                    },
                    y: {
                        stacked: true,
                        beginAtZero: true
                    }
                }
            }
        });
    }

    // 볼린저 신호 상세 차트 업데이트
    function updateBollingerSignalsChart() {
        const ctx = document.getElementById('bollinger-signals-chart').getContext('2d');
        
        // 기존 차트 제거
        if (bollingerSignalsChart) {
            bollingerSignalsChart.destroy();
        }
        
        // 데이터 준비
        const markets = Object.keys(signalsData.bollinger_by_market || {});
        const buyData = [];
        const neutralData = [];
        const sellData = [];
        
        markets.forEach(market => {
            const marketData = signalsData.bollinger_by_market[market];
            buyData.push(marketData['매수'] || 0);
            neutralData.push(marketData['중립'] || 0);
            sellData.push(marketData['매도'] || 0);
        });
        
        // 차트 생성
        bollingerSignalsChart = new Chart(ctx, {
            type: 'bar',
            data: {
                labels: markets,
                datasets: [
                    {
                        label: '매수',
                        data: buyData,
                        backgroundColor: 'rgba(40, 167, 69, 0.7)',
                        borderColor: 'rgb(40, 167, 69)',
                        borderWidth: 1
                    },
                    {
                        label: '중립',
                        data: neutralData,
                        backgroundColor: 'rgba(108, 117, 125, 0.7)',
                        borderColor: 'rgb(108, 117, 125)',
                        borderWidth: 1
                    },
                    {
                        label: '매도',
                        data: sellData,
                        backgroundColor: 'rgba(220, 53, 69, 0.7)',
                        borderColor: 'rgb(220, 53, 69)',
                        borderWidth: 1
                    }
                ]
            },
            options: {
                responsive: true,
                plugins: {
                    title: {
                        display: true,
                        text: '시장별 볼린저 밴드 신호 분포'
                    },
                    tooltip: {
                        mode: 'index',
                        intersect: false
                    }
                },
                scales: {
                    x: {
                        stacked: true,
                    },
                    y: {
                        stacked: true,
                        beginAtZero: true
                    }
                }
            }
        });
    }

    // 봇별 신호 차트 업데이트
    function updateBotSignalsCharts() {
        if (!signalsData || !signalsData.bot_signals || Object.keys(signalsData.bot_signals).length === 0) {
            return;
        }
        
        // 봇별 데이터 처리
        Object.entries(signalsData.bot_signals).forEach(([botEmail, signals]) => {
            const canvasId = `bot-signals-chart-${botEmail.replace(/[@.]/g, '-')}`;
            
            // 캔버스 요소가 없으면 생성
            let canvas = document.getElementById(canvasId);
            if (!canvas) {
                // 차트 컨테이너 생성
                const chartContainer = document.createElement('div');
                chartContainer.className = 'col-md-6 mb-4';
                chartContainer.innerHTML = `
                    <div class="card">
                        <div class="card-header">
                            <h6 class="mb-0">봇 신호 분포: ${botEmail}</h6>
                        </div>
                        <div class="card-body">
                            <canvas id="${canvasId}"></canvas>
                        </div>
                    </div>
                `;
                
                // 차트 컨테이너 추가
                const botsChartsContainer = document.getElementById('bot-signals-charts');
                if (botsChartsContainer) {
                    botsChartsContainer.appendChild(chartContainer);
                    canvas = document.getElementById(canvasId);
                }
            }
            
            if (canvas) {
                // 기존 차트 제거
                if (botSignalsCharts[botEmail]) {
                    botSignalsCharts[botEmail].destroy();
                }
                
                // 차트 데이터 준비
                const signalTypes = Object.keys(signals);
                const signalData = signalTypes.map(type => signals[type]);
                
                // 차트 색상 설정
                const backgroundColors = signalTypes.map(type => {
                    if (type === '매수') return 'rgba(40, 167, 69, 0.7)';
                    if (type === '중립') return 'rgba(108, 117, 125, 0.7)';
                    if (type === '매도') return 'rgba(220, 53, 69, 0.7)';
                    return `rgba(${Math.floor(Math.random() * 255)}, ${Math.floor(Math.random() * 255)}, ${Math.floor(Math.random() * 255)}, 0.7)`;
                });
                
                const borderColors = signalTypes.map(type => {
                    if (type === '매수') return 'rgb(40, 167, 69)';
                    if (type === '중립') return 'rgb(108, 117, 125)';
                    if (type === '매도') return 'rgb(220, 53, 69)';
                    return `rgb(${Math.floor(Math.random() * 255)}, ${Math.floor(Math.random() * 255)}, ${Math.floor(Math.random() * 255)})`;
                });
                
                // 차트 생성
                botSignalsCharts[botEmail] = new Chart(canvas.getContext('2d'), {
                    type: 'pie',
                    data: {
                        labels: signalTypes,
                        datasets: [{
                            data: signalData,
                            backgroundColor: backgroundColors,
                            borderColor: borderColors,
                            borderWidth: 1
                        }]
                    },
                    options: {
                        responsive: true,
                        plugins: {
                            legend: {
                                position: 'bottom',
                            }
                        }
                    }
                });
            }
        });
    }
    