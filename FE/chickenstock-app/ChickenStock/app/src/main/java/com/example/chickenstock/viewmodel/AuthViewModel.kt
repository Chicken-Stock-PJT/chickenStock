package com.example.chickenstock.viewmodel

import android.content.Context
import androidx.compose.runtime.State
import androidx.compose.runtime.mutableStateOf
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import com.example.chickenstock.data.TokenManager

class AuthViewModel(private val context: Context) : ViewModel() {
    private val _isLoggedIn = mutableStateOf(false)
    val isLoggedIn: State<Boolean> = _isLoggedIn

    private val _token = MutableStateFlow<String?>(null)

    fun getToken(): String? = _token.value

    init {
        // 앱 시작 시 저장된 로그인 상태 불러오기
        loadLoginState()
    }

    private fun loadLoginState() {
        val sharedPreferences = context.getSharedPreferences("auth_prefs", Context.MODE_PRIVATE)
        _isLoggedIn.value = sharedPreferences.getBoolean("is_logged_in", false)
    }

    fun login() {
        _isLoggedIn.value = true
        // 로그인 상태 저장
        saveLoginState(true)
    }

    fun logout() {
        _isLoggedIn.value = false
        _token.value = null
        // 토큰 삭제
        TokenManager.getInstance(context).clearTokens()
        // 로그인 상태 저장
        saveLoginState(false)
    }

    private fun saveLoginState(isLoggedIn: Boolean) {
        viewModelScope.launch {
            val sharedPreferences = context.getSharedPreferences("auth_prefs", Context.MODE_PRIVATE)
            sharedPreferences.edit().putBoolean("is_logged_in", isLoggedIn).apply()
        }
    }

    class Factory(private val context: Context) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T {
            if (modelClass.isAssignableFrom(AuthViewModel::class.java)) {
                return AuthViewModel(context) as T
            }
            throw IllegalArgumentException("Unknown ViewModel class")
        }
    }
} 