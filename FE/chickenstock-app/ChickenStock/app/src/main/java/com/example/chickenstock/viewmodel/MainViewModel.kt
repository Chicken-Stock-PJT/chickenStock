package com.example.chickenstock.viewmodel

import androidx.compose.runtime.State
import androidx.compose.runtime.mutableStateOf
import androidx.lifecycle.ViewModel
import androidx.lifecycle.viewModelScope
import com.example.chickenstock.model.PortfolioData
import com.example.chickenstock.model.StockUpdate
import com.example.chickenstock.api.MemberService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.WatchlistItem
import com.example.chickenstock.api.StockService
import com.example.chickenstock.api.SimpleProfileResponse
import com.example.chickenstock.api.RankingItem
import kotlinx.coroutines.launch
import android.content.Context
import android.util.Log
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import com.example.chickenstock.data.TokenManager
import com.example.chickenstock.model.TradeHistoryResponse
import com.example.chickenstock.model.TradeHistory
import com.example.chickenstock.api.TradeHistoryService

class MainViewModel : ViewModel() {
    private val _selectedIndex = mutableStateOf(0)
    val selectedIndex: State<Int> = _selectedIndex

    private val _isBottomBarVisible = mutableStateOf(true)
    val isBottomBarVisible: State<Boolean> = _isBottomBarVisible

    private val _isTopBarVisible = mutableStateOf(true)
    val isTopBarVisible: State<Boolean> = _isTopBarVisible

    private val _isLoading = MutableStateFlow(false)
    val isLoading: StateFlow<Boolean> = _isLoading.asStateFlow()

    private val _error = MutableStateFlow<String?>(null)
    val error: StateFlow<String?> = _error.asStateFlow()

    private val _userProfile = MutableStateFlow<SimpleProfileResponse?>(null)
    val userProfile: StateFlow<SimpleProfileResponse?> = _userProfile.asStateFlow()

    private val _stockRankings = MutableStateFlow<List<RankingItem>>(emptyList())
    val stockRankings: StateFlow<List<RankingItem>> = _stockRankings.asStateFlow()

    // 포트폴리오 데이터 상태
    private val _portfolioData = MutableStateFlow<PortfolioData?>(null)
    val portfolioData: StateFlow<PortfolioData?> = _portfolioData.asStateFlow()

    // 관심 종목 상태
    private val _watchlist = mutableStateOf<Set<String>>(emptySet())
    val watchlist: State<Set<String>> = _watchlist

    // 관심 종목 상세 정보
    private val _watchlistItems = MutableStateFlow<List<WatchlistItem>>(emptyList())
    val watchlistItems: StateFlow<List<WatchlistItem>> = _watchlistItems.asStateFlow()

    // 거래내역 상태
    private val _tradeHistoryList = MutableStateFlow<List<TradeHistory>>(emptyList())
    val tradeHistoryList: StateFlow<List<TradeHistory>> = _tradeHistoryList.asStateFlow()
    private val _realizedProfit = MutableStateFlow<Int?>(null)
    val realizedProfit: StateFlow<Int?> = _realizedProfit.asStateFlow()
    private val _hasNext = MutableStateFlow(false)
    val hasNext: StateFlow<Boolean> = _hasNext.asStateFlow()
    private val _nextCursor = MutableStateFlow<String?>(null)
    val nextCursor: StateFlow<String?> = _nextCursor.asStateFlow()

    private var memberService: MemberService? = null
    private var stockService: StockService? = null
    private var tradeHistoryService: TradeHistoryService? = null

    fun initializeServices(context: Context) {
        try {
            Log.d("MainViewModel", "서비스 초기화 시작")
            memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
            stockService = RetrofitClient.getInstance(context).create(StockService::class.java)
            Log.d("MainViewModel", "서비스 초기화 완료")
        } catch (e: Exception) {
            Log.e("MainViewModel", "서비스 초기화 중 오류 발생", e)
            _error.value = "서비스 초기화 중 오류가 발생했습니다."
        }
    }

    fun initializeTradeHistoryService(context: Context) {
        try {
            tradeHistoryService = RetrofitClient.getInstance(context).create(TradeHistoryService::class.java)
        } catch (e: Exception) {
            Log.e("MainViewModel", "거래내역 서비스 초기화 중 오류", e)
        }
    }

    fun loadUserProfile(memberService: MemberService, context: Context) {
        viewModelScope.launch {
            try {
                _isLoading.value = true
                _error.value = null
                Log.d("MainViewModel", "프로필 정보 조회 시작")
                val token = TokenManager.getInstance(context).getAccessToken()
                if (token.isNullOrBlank()) {
                    _error.value = "토큰이 없습니다. 다시 로그인 해주세요."
                    _isLoading.value = false
                    return@launch
                }
                val response = memberService.getSimpleProfile("Bearer $token")
                Log.d("MainViewModel", "프로필 정보 응답 코드: ${response.code()}")
                Log.d("MainViewModel", "프로필 정보 응답 메시지: ${response.message()}")
                
                if (response.isSuccessful) {
                    response.body()?.let { profile ->
                        Log.d("MainViewModel", "프로필 정보 조회 성공: ${profile.nickname}")
                        Log.d("MainViewModel", "프로필 상세 정보: memberMoney=${profile.memberMoney}, returnRate=${profile.returnRate}")
                        _userProfile.value = profile
                    } ?: run {
                        Log.e("MainViewModel", "프로필 정보 응답 본문이 null입니다.")
                        _error.value = "프로필 정보를 가져오는데 실패했습니다."
                    }
                } else {
                    Log.e("MainViewModel", "프로필 정보 조회 실패: ${response.code()} - ${response.message()}")
                    _error.value = "프로필 정보를 가져오는데 실패했습니다. (${response.code()})"
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "프로필 정보 조회 중 오류 발생", e)
                _error.value = "프로필 정보를 가져오는 중 오류가 발생했습니다: ${e.message}"
            } finally {
                _isLoading.value = false
            }
        }
    }

    fun loadPortfolio(memberService: MemberService, context: Context) {
        viewModelScope.launch {
            _isLoading.value = true
            try {
                val token = TokenManager.getInstance(context).getAccessToken()
                if (token.isNullOrBlank()) {
                    _error.value = "토큰이 없습니다. 다시 로그인 해주세요."
                    _isLoading.value = false
                    return@launch
                }
                val response = memberService.getPortfolio("Bearer $token")
                if (response.isSuccessful) {
                    _portfolioData.value = response.body()
                    _error.value = null
                } else {
                    _error.value = "포트폴리오 정보를 불러오는데 실패했습니다. (${response.code()})"
                }
            } catch (e: Exception) {
                _error.value = e.message ?: "알 수 없는 오류가 발생했습니다."
                Log.e("MainViewModel", "포트폴리오 로드 중 오류 발생", e)
            } finally {
                _isLoading.value = false
            }
        }
    }

    fun loadStockRankings(
        stockService: StockService,
        selectedSortIndex: Int,
        currentSortType: String
    ) {
        viewModelScope.launch {
            _isLoading.value = true
            _stockRankings.value = emptyList()
            try {
                Log.d("MainViewModel", "랭킹 데이터 로드 시작: sortIndex=$selectedSortIndex, sortType=$currentSortType")
                
                val response = when (selectedSortIndex) {
                    1, 2 -> stockService.getFluctuationRateRanking(
                        marketType = "000",
                        sortType = currentSortType
                    )
                    3 -> stockService.getVolumeRanking(
                        marketType = "000"
                    )
                    else -> stockService.getTradeAmountRanking(
                        marketType = "000"
                    )
                }

                if (response.isSuccessful) {
                    response.body()?.let { rankingResponse ->
                        Log.d("MainViewModel", "주식 랭킹 조회 성공: ${rankingResponse.rankingItems.size}개")
                        _stockRankings.value = rankingResponse.rankingItems.map { item ->
                            RankingItem(
                                stockCode = item.stockCode,
                                stockName = item.stockName,
                                currentPrice = item.currentPrice.toString(),
                                fluctuationRate = item.fluctuationRate.toString(),
                                tradeVolume = item.tradeVolume,
                                tradeAmount = item.tradeAmount
                            )
                        }
                        _error.value = null
                    } ?: run {
                        Log.e("MainViewModel", "랭킹 데이터가 비어있습니다")
                        _error.value = "데이터가 없습니다."
                    }
                } else {
                    Log.e("MainViewModel", "랭킹 데이터 로드 실패: ${response.code()}")
                    _error.value = "데이터를 불러오는데 실패했습니다. (${response.code()})"
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "랭킹 데이터 로드 중 오류 발생: ${e.message}", e)
                _error.value = when {
                    e.message?.contains("timeout") == true -> 
                        "서버 응답 시간이 초과되었습니다. 잠시 후 다시 시도해주세요."
                    e.message?.contains("Unable to resolve host") == true -> 
                        "인터넷 연결을 확인해주세요."
                    else -> "알 수 없는 오류가 발생했습니다: ${e.message}"
                }
            } finally {
                _isLoading.value = false
            }
        }
    }

    // 관심 종목 목록 로드
    fun loadWatchlist(context: Context) {
        viewModelScope.launch {
            _isLoading.value = true
            try {
                val token = TokenManager.getInstance(context).getAccessToken()
                if (token.isNullOrBlank()) {
                    _error.value = "토큰이 없습니다. 다시 로그인 해주세요."
                    _isLoading.value = false
                    return@launch
                }
                
                // memberService가 null인 경우 초기화
                if (memberService == null) {
                    Log.d("MainViewModel", "memberService가 null이므로 초기화합니다.")
                    initializeServices(context)
                }
                
                memberService?.let { service ->
                    val response = service.getWatchlist("Bearer $token")
                    if (response.isSuccessful) {
                        response.body()?.let { watchlistResponse ->
                            Log.d("MainViewModel", "관심 종목 조회 성공: ${watchlistResponse.watchList.size}개")
                            Log.d("MainViewModel", "관심 종목 상세 정보: ${watchlistResponse.watchList}")
                            _watchlistItems.value = watchlistResponse.watchList
                            _watchlist.value = watchlistResponse.watchList.map { it.stockCode }.toSet()
                        }
                    } else {
                        Log.e("MainViewModel", "관심 종목 조회 실패: ${response.code()} - ${response.message()}")
                        _error.value = "관심 종목 조회에 실패했습니다. (${response.code()})"
                    }
                } ?: run {
                    Log.e("MainViewModel", "memberService 초기화 실패")
                    _error.value = "서비스가 초기화되지 않았습니다."
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "관심 종목 조회 오류", e)
                _error.value = e.message ?: "알 수 없는 오류가 발생했습니다."
            } finally {
                _isLoading.value = false
            }
        }
    }

    // 관심 종목 상세 정보 가져오기
    fun getWatchlistItem(stockCode: String): WatchlistItem? {
        return _watchlistItems.value.find { it.stockCode == stockCode }
    }

    // 관심 종목 추가
    fun addToWatchlist(stockCode: String, context: Context, onSuccess: () -> Unit = {}, onError: (String) -> Unit = {}) {
        viewModelScope.launch {
            try {
                val token = TokenManager.getInstance(context).getAccessToken()
                if (token.isNullOrBlank()) {
                    onError("토큰이 없습니다. 다시 로그인 해주세요.")
                    return@launch
                }
                
                // memberService가 null인 경우 초기화
                if (memberService == null) {
                    Log.d("MainViewModel", "memberService가 null이므로 초기화합니다.")
                    initializeServices(context)
                }
                
                memberService?.addToWatchlist("Bearer $token", stockCode)?.let { response ->
                    if (response.isSuccessful) {
                        Log.d("MainViewModel", "관심 종목 추가 성공: $stockCode")
                        _watchlist.value = _watchlist.value + stockCode
                        
                        // 관심 목록 새로고침
                        loadWatchlist(context)
                        
                        onSuccess()
                    } else {
                        Log.e("MainViewModel", "관심 종목 추가 실패: ${response.code()} - ${response.message()}")
                        onError("관심 종목 추가에 실패했습니다. (${response.code()})")
                    }
                } ?: run {
                    Log.e("MainViewModel", "memberService 초기화 실패")
                    onError("서비스가 초기화되지 않았습니다.")
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "관심 종목 추가 오류", e)
                onError(e.message ?: "관심 종목 추가 중 오류가 발생했습니다.")
            }
        }
    }

    // 관심 종목 제거
    fun removeFromWatchlist(stockCode: String, context: Context, onSuccess: () -> Unit = {}, onError: (String) -> Unit = {}) {
        viewModelScope.launch {
            try {
                val token = TokenManager.getInstance(context).getAccessToken()
                if (token.isNullOrBlank()) {
                    onError("토큰이 없습니다. 다시 로그인 해주세요.")
                    return@launch
                }
                
                // memberService가 null인 경우 초기화
                if (memberService == null) {
                    Log.d("MainViewModel", "memberService가 null이므로 초기화합니다.")
                    initializeServices(context)
                }
                
                memberService?.removeFromWatchlist("Bearer $token", stockCode)?.let { response ->
                    if (response.isSuccessful) {
                        Log.d("MainViewModel", "관심 종목 삭제 성공: $stockCode")
                        _watchlist.value = _watchlist.value - stockCode
                        
                        // 관심 목록 새로고침
                        loadWatchlist(context)
                        
                        onSuccess()
                    } else {
                        Log.e("MainViewModel", "관심 종목 삭제 실패: ${response.code()} - ${response.message()}")
                        onError("관심 종목 삭제에 실패했습니다. (${response.code()})")
                    }
                } ?: run {
                    Log.e("MainViewModel", "memberService 초기화 실패")
                    onError("서비스가 초기화되지 않았습니다.")
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "관심 종목 삭제 오류", e)
                onError(e.message ?: "관심 종목 삭제 중 오류가 발생했습니다.")
            }
        }
    }

    fun updateSelectedIndex(index: Int) {
        _selectedIndex.value = index
    }

    fun setBottomBarVisibility(isVisible: Boolean) {
        _isBottomBarVisible.value = isVisible
    }

    fun setTopBarVisibility(visible: Boolean) {
        _isTopBarVisible.value = visible
    }

    // 웹소켓으로부터 받은 업데이트로 포트폴리오 데이터 갱신
    fun updatePortfolioData(stockUpdate: StockUpdate) {
        _portfolioData.value?.let { currentData ->
            _portfolioData.value = currentData.copy(
                totalAsset = stockUpdate.totalData.totalAsset,
                totalProfitLoss = stockUpdate.totalData.totalProfitLoss,
                totalReturnRate = stockUpdate.totalData.totalReturnRate,
                positions = currentData.positions?.map { position ->
                    if (position.stockCode == stockUpdate.stockCode) {
                        position.copy(
                            currentPrice = stockUpdate.currentPrice,
                            valuationAmount = stockUpdate.valuationAmount,
                            profitLoss = stockUpdate.profitLoss,
                            returnRate = stockUpdate.returnRate
                        )
                    } else {
                        position
                    }
                } ?: emptyList()
            )
        }
    }

    // 모든 상태 초기화 (새로고침 시 사용)
    fun clearAllStates() {
        _userProfile.value = null
        _portfolioData.value = null
        _watchlistItems.value = emptyList()
        _watchlist.value = emptySet()
        _stockRankings.value = emptyList()
        _error.value = null
    }

    fun loadTradeHistories(context: Context, size: Int = 20, cursor: String? = null) {
        viewModelScope.launch {
            _isLoading.value = true
            try {
                val token = TokenManager.getInstance(context).getAccessToken()
                if (token.isNullOrBlank()) {
                    _error.value = "토큰이 없습니다. 다시 로그인 해주세요."
                    _isLoading.value = false
                    return@launch
                }
                if (tradeHistoryService == null) {
                    initializeTradeHistoryService(context)
                }
                val response = tradeHistoryService?.getTradeHistories("Bearer $token", size, cursor)
                response?.let {
                    if (cursor == null) {
                        _tradeHistoryList.value = it.tradeHistories
                    } else {
                        _tradeHistoryList.value = _tradeHistoryList.value + it.tradeHistories
                    }
                    _realizedProfit.value = it.realizedProfit
                    _hasNext.value = it.hasNext
                    _nextCursor.value = it.nextCursor
                    _error.value = null
                }
            } catch (e: Exception) {
                _error.value = e.message ?: "거래내역을 불러오는 중 오류가 발생했습니다."
                Log.e("MainViewModel", "거래내역 로드 중 오류 발생", e)
            } finally {
                _isLoading.value = false
            }
        }
    }

    fun loadData(stockService: StockService, memberService: MemberService, context: Context) {
        viewModelScope.launch {
            try {
                _isLoading.value = true
                _error.value = null
                
                // 토큰 정보 확인
                val tokenManager = TokenManager.getInstance(context)
                val accessToken = tokenManager.getAccessToken()
                
                if (accessToken == null) {
                    Log.e("MainViewModel", "액세스 토큰이 없습니다. 로그인이 필요합니다.")
                    _error.value = "로그인이 필요합니다."
                    _isLoading.value = false
                    return@launch
                }
                
                // 병렬로 데이터 로드
                try {
                    Log.d("MainViewModel", "프로필 정보 조회 시작")
                    loadUserProfile(memberService, context)
                    
                    Log.d("MainViewModel", "포트폴리오 조회 시작")
                    loadPortfolio(memberService, context)
                    
                    Log.d("MainViewModel", "관심종목 조회 시작")
                    loadWatchlist(context)
                    
                    // 거래대금 API 호출
                    Log.d("MainViewModel", "거래대금 랭킹 조회 시작")
                    loadStockRankings(stockService, 0, "1")
                } catch (e: Exception) {
                    Log.e("MainViewModel", "데이터 로드 중 오류 발생: ${e.message}", e)
                    _error.value = "데이터를 불러오는 중 오류가 발생했습니다."
                } finally {
                    _isLoading.value = false
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "데이터 로드 준비 중 오류 발생: ${e.message}", e)
                _error.value = "데이터 로드를 준비하는 중 오류가 발생했습니다."
                _isLoading.value = false
            }
        }
    }
} 