from prometheus_client import Counter, Gauge, Histogram, Summary
import time

# 시스템 메트릭
system_up = Gauge('trading_system_up', 'Indicates if the trading system is running')
system_uptime = Gauge('trading_system_uptime_seconds', 'Total uptime in seconds')
system_start_time = time.time()
error_count = Counter('trading_system_errors_total', 'Total number of errors', ['severity', 'module'])

# 봇 관련 메트릭
active_bots = Gauge('trading_active_bots_total', 'Total number of active trading bots')
bot_status = Gauge('trading_bot_status', 'Bot running status (1=running, 0=stopped)', ['email', 'strategy'])
bot_uptime = Gauge('trading_bot_uptime_seconds', 'Bot uptime in seconds', ['email'])
bot_creation_count = Counter('trading_bot_creation_total', 'Total number of bot creation attempts', ['success'])
bot_start_count = Counter('trading_bot_start_total', 'Total number of bot start attempts', ['success'])
bot_stop_count = Counter('trading_bot_stop_total', 'Total number of bot stop attempts', ['success'])

# 거래 관련 메트릭
trade_signals = Counter('trading_signals_total', 'Total number of trading signals', ['strategy', 'signal_type'])
trade_signal_latency = Histogram('trading_signal_latency_seconds', 'Time between signal generation and processing', ['strategy'])
trades_executed = Counter('trading_trades_executed_total', 'Total number of executed trades', ['strategy', 'action'])
trade_execution_latency = Histogram('trading_trade_execution_latency_seconds', 'Time taken to execute a trade', ['action'])
trade_volume = Counter('trading_volume_total', 'Total trading volume in shares', ['action'])
trade_amount = Counter('trading_amount_total', 'Total trading amount in KRW', ['action'])
trade_success_rate = Gauge('trading_success_rate', 'Success rate of trade executions', ['strategy'])

# 계좌 관련 메트릭
account_balance = Gauge('trading_account_cash_balance', 'Cash balance in account', ['email'])
total_asset_value = Gauge('trading_account_total_asset', 'Total asset value', ['email'])
holding_count = Gauge('trading_account_holdings_count', 'Number of stocks in holdings', ['email'])
profit_loss = Gauge('trading_account_profit_loss', 'Current profit/loss in KRW', ['email'])
profit_loss_rate = Gauge('trading_account_profit_loss_rate', 'Current profit/loss rate in percentage', ['email'])

# 데이터 관련 메트릭
data_update_time = Gauge('trading_data_last_update_timestamp', 'Timestamp of last data update')
data_freshness = Gauge('trading_data_freshness_seconds', 'Seconds since last data update')
realtime_data_count = Counter('trading_realtime_data_total', 'Total number of realtime data updates')
realtime_data_latency = Histogram('trading_realtime_data_latency_seconds', 'Latency of realtime data processing in seconds')
chart_data_completeness = Gauge('trading_chart_data_completeness', 'Completeness of chart data in percentage', ['market'])

# API 관련 메트릭
api_requests = Counter('trading_api_requests_total', 'Total number of API requests', ['api_name', 'endpoint'])
api_errors = Counter('trading_api_errors_total', 'Total number of API errors', ['api_name', 'error_type'])
api_request_duration = Histogram('trading_api_request_duration_seconds', 'Duration of API requests', ['api_name'],
                                buckets=[0.01, 0.05, 0.1, 0.5, 1.0, 2.0, 5.0, 10.0])
api_request_in_progress = Gauge('trading_api_requests_in_progress', 'Number of API requests in progress', ['api_name'])

# 전략 관련 메트릭
strategy_performance = Gauge('trading_strategy_performance', 'Performance of trading strategy in percentage', ['strategy', 'timeframe'])
strategy_win_rate = Gauge('trading_strategy_win_rate', 'Win rate of trading strategy (profitable trades / total trades)', ['strategy'])
strategy_indicator_calculation_time = Histogram('trading_strategy_indicator_calculation_seconds', 
                                               'Time taken to calculate strategy indicators', ['strategy'])

# 시장 관련 메트릭
market_index_value = Gauge('trading_market_index_value', 'Current value of market index', ['index_name'])
market_index_change = Gauge('trading_market_index_change', 'Daily change rate of market indices in percentage', ['index_name'])
filtered_stocks_count = Gauge('trading_filtered_stocks_count', 'Number of stocks after filtering', ['market'])