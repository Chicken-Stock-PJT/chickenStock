package com.example.chickenstock.viewmodel

import androidx.compose.runtime.State
import androidx.compose.runtime.mutableStateOf
import androidx.lifecycle.ViewModel

class MainViewModel : ViewModel() {
    private val _selectedIndex = mutableStateOf(0)
    val selectedIndex: State<Int> = _selectedIndex

    private val _isBottomBarVisible = mutableStateOf(true)
    val isBottomBarVisible: State<Boolean> = _isBottomBarVisible

    private val _isTopBarVisible = mutableStateOf(true)
    val isTopBarVisible: State<Boolean> = _isTopBarVisible

    fun updateSelectedIndex(index: Int) {
        _selectedIndex.value = index
    }

    fun setBottomBarVisibility(visible: Boolean) {
        _isBottomBarVisible.value = visible
    }

    fun setTopBarVisibility(visible: Boolean) {
        _isTopBarVisible.value = visible
    }
} 