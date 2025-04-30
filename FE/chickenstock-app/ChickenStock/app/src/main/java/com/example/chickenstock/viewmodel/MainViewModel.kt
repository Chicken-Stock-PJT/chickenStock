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
import kotlinx.coroutines.launch
import android.content.Context
import android.util.Log

class MainViewModel : ViewModel() {
    private val _selectedIndex = mutableStateOf(0)
    val selectedIndex: State<Int> = _selectedIndex

    private val _isBottomBarVisible = mutableStateOf(true)
    val isBottomBarVisible: State<Boolean> = _isBottomBarVisible

    private val _isTopBarVisible = mutableStateOf(true)
    val isTopBarVisible: State<Boolean> = _isTopBarVisible

    // 포트폴리오 데이터 상태
    private val _portfolioData = mutableStateOf<PortfolioData?>(null)
    val portfolioData: State<PortfolioData?> = _portfolioData

    // 관심 종목 상태
    private val _watchlist = mutableStateOf<Set<String>>(emptySet())
    val watchlist: State<Set<String>> = _watchlist

    // 관심 종목 상세 정보
    private val _watchlistItems = mutableStateOf<List<WatchlistItem>>(emptyList())
    val watchlistItems: State<List<WatchlistItem>> = _watchlistItems

    private var memberService: MemberService? = null

    fun initializeServices(context: Context) {
        memberService = RetrofitClient.getInstance(context).create(MemberService::class.java)
    }

    // 관심 종목 목록 로드
    fun loadWatchlist() {
        viewModelScope.launch {
            try {
                Log.d("MainViewModel", "관심 종목 조회 시작")
                val response = memberService?.getWatchlist()
                if (response?.isSuccessful == true) {
                    response.body()?.let { watchlistResponse ->
                        Log.d("MainViewModel", "관심 종목 조회 성공: ${watchlistResponse.watchList.size}개")
                        Log.d("MainViewModel", "관심 종목 목록: ${watchlistResponse.watchList}")
                        _watchlistItems.value = watchlistResponse.watchList
                        _watchlist.value = watchlistResponse.watchList.map { it.stockCode }.toSet()
                    }
                } else {
                    Log.e("MainViewModel", "관심 종목 조회 실패: ${response?.code()} - ${response?.message()}")
                }
            } catch (e: Exception) {
                Log.e("MainViewModel", "관심 종목 조회 오류", e)
            }
        }
    }

    // 관심 종목 상세 정보 가져오기
    fun getWatchlistItem(stockCode: String): WatchlistItem? {
        return _watchlistItems.value.find { it.stockCode == stockCode }
    }

    // 관심 종목 추가
    fun addToWatchlist(stockCode: String, onSuccess: () -> Unit = {}, onError: (String) -> Unit = {}) {
        viewModelScope.launch {
            try {
                memberService?.addToWatchlist(stockCode)?.let { response ->
                    if (response.isSuccessful) {
                        _watchlist.value = _watchlist.value + stockCode
                        onSuccess()
                    } else {
                        onError("관심 종목 추가에 실패했습니다.")
                    }
                }
            } catch (e: Exception) {
                onError(e.message ?: "관심 종목 추가 중 오류가 발생했습니다.")
            }
        }
    }

    // 관심 종목 제거
    fun removeFromWatchlist(stockCode: String, onSuccess: () -> Unit = {}, onError: (String) -> Unit = {}) {
        viewModelScope.launch {
            try {
                memberService?.removeFromWatchlist(stockCode)?.let { response ->
                    if (response.isSuccessful) {
                        _watchlist.value = _watchlist.value - stockCode
                        onSuccess()
                    } else {
                        onError("관심 종목 삭제에 실패했습니다.")
                    }
                }
            } catch (e: Exception) {
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

    // 포트폴리오 데이터 설정
    fun setPortfolioData(data: PortfolioData?) {
        _portfolioData.value = data
    }

    // 웹소켓으로부터 받은 업데이트로 포트폴리오 데이터 갱신
    fun updatePortfolioData(stockUpdate: StockUpdate) {
        _portfolioData.value = _portfolioData.value?.copy(
            totalAsset = stockUpdate.totalData.totalAsset,
            totalProfitLoss = stockUpdate.totalData.totalProfitLoss,
            totalReturnRate = stockUpdate.totalData.totalReturnRate,
            positions = _portfolioData.value?.positions?.map { position ->
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